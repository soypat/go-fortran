package fortio

import (
	"bytes"
	"io"
	"math/rand"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"time"

	"github.com/soypat/go-fortran/intrinsic"
)

// unitState tracks connection info for each open unit.
type unitState struct {
	file        *os.File
	name        string
	access      AccessMode
	form        FormMode
	action      ActionMode
	position    PositionMode
	recl        int32
	blank       BlankMode
	delim       DelimMode
	pad         PadMode
	decimal     DecimalMode
	round       RoundMode
	sign        SignMode
	encoding    EncodingMode
	async       AsyncMode
	scratch     bool  // is scratch file
	pos         int64 // current byte position (stream)
	rec         int32 // current record number (direct)
	lastLineLen int   // last read line length (optimization for fixed-width files)
}

// Environment manages Fortran runtime state including I/O units.
type Environment struct {
	// Cached units 0-7 (0=stderr, 5=stdin, 6=stdout).
	unitsCache [8]*unitState
	// All other units.
	units map[int32]*unitState
	// Last error information.
	lastStat IOStat
	lastMsg  string
	// Scratch buffer for I/O operations.
	buf []byte
	// startTime is used by CpuTime to compute elapsed processor time.
	startTime time.Time
}

// NewEnvironment creates a new Fortran runtime environment
// with standard units pre-connected.
func NewEnvironment() *Environment {
	env := &Environment{
		units:     make(map[int32]*unitState),
		startTime: time.Now(),
	}
	// Unit 0: stderr
	env.unitsCache[0] = &unitState{
		file:   os.Stderr,
		name:   "stderr",
		access: AccessSEQUENTIAL,
		form:   FormFORMATTED,
		action: ActionWRITE,
	}
	// Unit 5: stdin
	env.unitsCache[5] = &unitState{
		file:   os.Stdin,
		name:   "stdin",
		access: AccessSEQUENTIAL,
		form:   FormFORMATTED,
		action: ActionREAD,
	}
	// Unit 6: stdout
	env.unitsCache[6] = &unitState{
		file:   os.Stdout,
		name:   "stdout",
		access: AccessSEQUENTIAL,
		form:   FormFORMATTED,
		action: ActionWRITE,
	}
	return env
}

// getUnit returns the unit state for a unit number, or nil if not connected.
func (env *Environment) getUnit(unit int32) *unitState {
	if unit >= 0 && int(unit) < len(env.unitsCache) {
		return env.unitsCache[unit]
	}
	return env.units[unit]
}

// setUnit stores unit state for a unit number.
func (env *Environment) setUnit(unit int32, state *unitState) {
	if unit >= 0 && int(unit) < len(env.unitsCache) {
		env.unitsCache[unit] = state
	} else {
		env.units[unit] = state
	}
}

// removeUnit removes unit state for a unit number.
func (env *Environment) removeUnit(unit int32) {
	if unit >= 0 && int(unit) < len(env.unitsCache) {
		env.unitsCache[unit] = nil
	} else {
		delete(env.units, unit)
	}
}

// Open connects a file to a unit number per OPEN statement semantics.
func (env *Environment) Open(spec OpenSpec) IOStat {
	// Close existing connection if any
	if existing := env.getUnit(spec.UNIT); existing != nil {
		env.Close(CloseSpec{UNIT: spec.UNIT})
	}

	// Determine file flags based on status and action
	var flag int
	switch spec.ACTION {
	case ActionREAD:
		flag = os.O_RDONLY
	case ActionWRITE:
		flag = os.O_WRONLY
	default: // ActionREADWRITE
		flag = os.O_RDWR
	}

	switch spec.STATUS {
	case StatusOLD:
		// File must exist
	case StatusNEW:
		flag |= os.O_CREATE | os.O_EXCL
	case StatusREPLACE:
		flag |= os.O_CREATE | os.O_TRUNC
	case StatusSCRATCH:
		flag |= os.O_CREATE | os.O_EXCL
		// TODO: create temp file
	default: // StatusUNKNOWN
		flag |= os.O_CREATE
	}

	f, err := os.OpenFile(strings.TrimRight(spec.FILE, " "), flag, 0644)
	if err != nil {
		stat := env.mapError(err)
		if spec.IOSTAT != nil {
			*spec.IOSTAT = int32(stat)
		}
		if spec.IOMSG != nil {
			spec.IOMSG.SetFromString(err.Error())
		}
		return stat
	}

	// Handle position
	switch spec.POSITION {
	case PositionREWIND:
		f.Seek(0, io.SeekStart)
	case PositionAPPEND:
		f.Seek(0, io.SeekEnd)
	}

	state := &unitState{
		file:     f,
		name:     spec.FILE,
		access:   spec.ACCESS,
		form:     spec.FORM,
		action:   spec.ACTION,
		position: spec.POSITION,
		recl:     spec.RECL,
		blank:    spec.BLANK,
		delim:    spec.DELIM,
		pad:      spec.PAD,
		decimal:  spec.DECIMAL,
		round:    spec.ROUND,
		sign:     spec.SIGN,
		encoding: spec.ENCODING,
		async:    spec.ASYNCHRONOUS,
		scratch:  spec.STATUS == StatusSCRATCH,
	}
	if spec.IOSTAT != nil {
		*spec.IOSTAT = int32(IOStatOK)
	}
	env.setUnit(spec.UNIT, state)
	return IOStatOK
}

// Close disconnects a unit per CLOSE statement semantics.
func (env *Environment) Close(spec CloseSpec) IOStat {
	state := env.getUnit(spec.UNIT)
	if state == nil {
		// Not connected - not an error
		if spec.IOSTAT != nil {
			*spec.IOSTAT = int32(IOStatOK)
		}
		return IOStatOK
	}

	// Close the file
	var err error
	if state.file != nil {
		err = state.file.Close()
	}

	// Delete if requested or scratch file
	shouldDelete := spec.STATUS == CloseDELETE || state.scratch
	if shouldDelete && state.name != "" {
		os.Remove(state.name)
	}

	env.removeUnit(spec.UNIT)

	if err != nil {
		stat := env.mapError(err)
		if spec.IOSTAT != nil {
			*spec.IOSTAT = int32(stat)
		}
		if spec.IOMSG != nil {
			spec.IOMSG.SetFromString(err.Error())
		}
		return stat
	}

	if spec.IOSTAT != nil {
		*spec.IOSTAT = int32(IOStatOK)
	}
	return IOStatOK
}

// Inquire queries properties of a unit or file per INQUIRE statement.
func (env *Environment) Inquire(spec InquireSpec) InquireResult {
	var result InquireResult

	if spec.ByFile {
		// Inquire by file name
		info, err := os.Stat(spec.FILE)
		if err != nil {
			result.EXIST = false
			result.IOSTAT = IOStatOK
			return result
		}
		result.EXIST = true
		result.NAMED = true
		result.NAME = spec.FILE
		result.SIZE = info.Size()
		// File capabilities (assume all for regular files)
		result.SEQUENTIAL = InquiryYES
		result.DIRECT = InquiryYES
		result.STREAM = InquiryYES
		result.FORMATTED = InquiryYES
		result.UNFORMATTED = InquiryYES
		result.READ = InquiryYES
		result.WRITE = InquiryYES
		result.READWRITE = InquiryYES
		result.IOSTAT = IOStatOK
		return result
	}

	// Inquire by unit
	state := env.getUnit(spec.UNIT)
	if state == nil {
		result.EXIST = true // Unit number exists conceptually
		result.OPENED = false
		result.NUMBER = -1
		result.IOSTAT = IOStatOK
		return result
	}

	result.EXIST = true
	result.OPENED = true
	result.NUMBER = spec.UNIT
	result.NAMED = !state.scratch
	result.NAME = state.name

	result.ACCESS = state.access
	result.FORM = state.form
	result.POSITION = state.position
	result.ACTION = state.action
	result.BLANK = state.blank
	result.DELIM = state.delim
	result.PAD = state.pad
	result.DECIMAL = state.decimal
	result.ROUND = state.round
	result.SIGN = state.sign
	result.ENCODING = state.encoding
	result.ASYNCHRONOUS = state.async

	result.RECL = state.recl
	result.POS = state.pos

	// Determine capabilities based on current mode
	result.SEQUENTIAL = InquiryYES
	result.DIRECT = InquiryYES
	result.STREAM = InquiryYES
	result.FORMATTED = InquiryYES
	result.UNFORMATTED = InquiryYES

	switch state.action {
	case ActionREAD:
		result.READ = InquiryYES
		result.WRITE = InquiryNO
		result.READWRITE = InquiryNO
	case ActionWRITE:
		result.READ = InquiryNO
		result.WRITE = InquiryYES
		result.READWRITE = InquiryNO
	default:
		result.READ = InquiryYES
		result.WRITE = InquiryYES
		result.READWRITE = InquiryYES
	}

	result.IOSTAT = IOStatOK
	return result
}

// Rewind positions a sequential unit at the beginning.
func (env *Environment) Rewind(unit int32) IOStat {
	state := env.getUnit(unit)
	if state == nil {
		return IOStatErrNotConnected
	}
	if state.file != nil {
		state.file.Seek(0, io.SeekStart)
	}
	state.pos = 0
	state.rec = 0
	return IOStatOK
}

// Backspace positions a sequential unit before the previous record.
func (env *Environment) Backspace(unit int32) IOStat {
	state := env.getUnit(unit)
	if state == nil {
		return IOStatErrNotConnected
	}
	// TODO: implement proper backspace logic for formatted files
	return IOStatOK
}

// Endfile writes an end-of-file marker.
func (env *Environment) Endfile(unit int32) IOStat {
	state := env.getUnit(unit)
	if state == nil {
		return IOStatErrNotConnected
	}
	// For direct access, truncate at current position
	if state.file != nil {
		pos, _ := state.file.Seek(0, io.SeekCurrent)
		state.file.Truncate(pos)
	}
	return IOStatOK
}

// Flush forces buffered data to be written.
func (env *Environment) Flush(unit int32) IOStat {
	state := env.getUnit(unit)
	if state == nil {
		return IOStatErrNotConnected
	}
	if state.file != nil {
		state.file.Sync()
	}
	return IOStatOK
}

// Write performs formatted output to a unit.
func (env *Environment) Write(unit int32, f *Format, args ...any) IOStat {
	state := env.getUnit(unit)
	if state == nil {
		return IOStatErrNotConnected
	}
	if state.file == nil {
		return IOStatErrNotConnected
	}

	buf := env.buf[:0]

	// Check if we have a format specification (either raw spec or pre-parsed)
	if f.spec != "" || f.parsed {
		// Formatted output using format descriptors
		f.ensureParsed()
		buf = f.writeFormatted(buf, args)
	} else {
		// List-directed output (PRINT * behavior)
		buf = f.writeListDirected(buf, args)
	}

	buf = append(buf, '\n')
	_, err := state.file.Write(buf)
	env.buf = buf[:0] // keep buffer if expanded

	if err != nil {
		stat := env.mapError(err)
		env.lastStat = stat
		env.lastMsg = err.Error()
		return stat
	}
	return IOStatOK
}

// WriteWithSpec performs formatted output with full I/O specifier support.
func (env *Environment) WriteWithSpec(spec IOSpec, args ...any) IOStat {
	stat := env.Write(spec.UNIT, spec.FMT, args...)
	if spec.IOSTAT != nil {
		*spec.IOSTAT = int32(stat)
	}
	return stat
}

// Read performs formatted input from a unit.
func (env *Environment) Read(unit int32, f *Format, args ...any) IOStat {
	state := env.getUnit(unit)
	if state == nil {
		return IOStatErrNotConnected
	}
	if state.file == nil {
		return IOStatErrNotConnected
	}

	line, err := env.appendLine(nil, state)
	if err != nil {
		stat := env.mapError(err)
		env.lastStat = stat
		env.lastMsg = err.Error()
		return stat
	}
	lineStr := string(line)

	// Parse based on format
	var parseErr error
	if f.spec == "" && !f.parsed {
		// List-directed input
		parseErr = readListDirected(lineStr, args)
	} else {
		f.ensureParsed()
		parseErr = readFormatted(lineStr, f.descriptors, args)
	}

	if parseErr != nil {
		env.lastStat = IOStatErrConversion
		env.lastMsg = parseErr.Error()
		return IOStatErrConversion
	}
	return IOStatOK
}

// ReadWithSpec performs formatted input with full I/O specifier support.
func (env *Environment) ReadWithSpec(spec IOSpec, args ...any) IOStat {
	stat := env.Read(spec.UNIT, spec.FMT, args...)
	if spec.IOSTAT != nil {
		*spec.IOSTAT = int32(stat)
	}
	return stat
}

// appendLine reads a line from state and appends it to dst.
func (env *Environment) appendLine(dst []byte, state *unitState) ([]byte, error) {
	readLen := max(state.lastLineLen+1, 512)
	buf := make([]byte, readLen)
	for {
		n, err := state.file.Read(buf[:readLen])
		if n > 0 {
			idx := bytes.IndexByte(buf[:n], '\n')
			if idx >= 0 {
				if extra := n - idx - 1; extra > 0 {
					state.file.Seek(int64(-extra), io.SeekCurrent)
				}
				dst = append(dst, buf[:idx]...)
				state.lastLineLen = len(dst)
				return dst, nil
			}
			dst = append(dst, buf[:n]...)
		}
		if err != nil {
			if err == io.EOF && len(dst) > 0 {
				state.lastLineLen = len(dst)
				return dst, nil
			}
			return dst, err
		}
	}
}

// Print performs list-directed output to stdout (unit 6).
func (env *Environment) Print(args ...any) IOStat {
	return env.Write(6, DefaultFormat(), args...)
}

// PrintFmt performs formatted output to stdout (unit 6).
func (env *Environment) PrintFmt(f *Format, args ...any) IOStat {
	return env.Write(6, f, args...)
}

// LastError returns the last error status and message.
func (env *Environment) LastError() (IOStat, string) {
	return env.lastStat, env.lastMsg
}

// Stop terminates the program with a STOP statement.
// Writes "STOP <code>" to stdout and exits with the given code.
func (env *Environment) Stop(code int) {
	msg := "STOP " + strconv.Itoa(code)
	state := env.getUnit(6) // stdout
	if state != nil && state.file != nil {
		state.file.WriteString(msg)
		state.file.Sync()
	}
	os.Exit(code)
}

// mapError converts a Go error to an IOStat value.
func (env *Environment) mapError(err error) IOStat {
	if err == nil {
		return IOStatOK
	}
	if err == io.EOF {
		return IOStatEOF
	}
	if os.IsNotExist(err) {
		return IOStatErrFileNotFound
	}
	if os.IsPermission(err) {
		return IOStatErrPermissionDenied
	}
	if os.IsExist(err) {
		return IOStatErrFileExists
	}
	return IOStatErrInternal
}

// Exit terminates the program with an optional exit code (default 0).
func (env *Environment) Exit(code ...int32) {
	if len(code) > 0 {
		os.Exit(int(code[0]))
	}
	os.Exit(0)
}

// System executes a shell command via "sh -c". Returns the exit code.
func (env *Environment) System(cmd string) int32 {
	c := exec.Command("sh", "-c", cmd)
	c.Stdout = os.Stdout
	c.Stderr = os.Stderr
	if err := c.Run(); err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			return int32(exitErr.ExitCode())
		}
		return 1
	}
	return 0
}

// CpuTime sets t to an approximation of processor time used in seconds.
func (env *Environment) CpuTime(t ...*float32) {
	if len(t) > 0 && t[0] != nil {
		*t[0] = float32(time.Since(env.startTime).Seconds())
	}
}

// SystemClock returns system clock data. Nil pointers are ignored.
// COUNT is the tick count, COUNT_RATE is ticks per second, COUNT_MAX is the max tick value.
func (env *Environment) SystemClock(count, countRate, countMax *int32) {
	if count != nil {
		*count = int32(time.Now().UnixMilli())
	}
	if countRate != nil {
		*countRate = 1000
	}
	if countMax != nil {
		*countMax = int32(^uint32(0) >> 1)
	}
}

// DateAndTime fills date ("YYYYMMDD"), time_ ("hhmmss.sss"), zone ("+hhmm") strings.
// Nil pointers are ignored.
func (env *Environment) DateAndTime(date, time_, zone *intrinsic.CharacterArray) {
	now := time.Now()
	if date != nil {
		date.SetFromString(now.Format("20060102"))
	}
	if time_ != nil {
		time_.SetFromString(now.Format("150405.000"))
	}
	if zone != nil {
		_, offset := now.Zone()
		h, m := offset/3600, (offset%3600)/60
		if m < 0 {
			m = -m
		}
		sign := "+"
		if offset < 0 {
			sign = "-"
			h = -h
		}
		zone.SetFromString(sign + strconv.Itoa(h/10) + strconv.Itoa(h%10) +
			strconv.Itoa(m/10) + strconv.Itoa(m%10))
	}
}

// DateAndTimeValues fills date/time/zone strings and an 8-element integer values array.
// values: (year, month, day, utc_offset_minutes, hour, minute, second, millisecond)
func (env *Environment) DateAndTimeValues(date, time_, zone *intrinsic.CharacterArray, values *intrinsic.Array[int32]) {
	env.DateAndTime(date, time_, zone)
	if values != nil {
		now := time.Now()
		_, offset := now.Zone()
		values.Set(int32(now.Year()), 1)
		values.Set(int32(now.Month()), 2)
		values.Set(int32(now.Day()), 3)
		values.Set(int32(offset/60), 4)
		values.Set(int32(now.Hour()), 5)
		values.Set(int32(now.Minute()), 6)
		values.Set(int32(now.Second()), 7)
		values.Set(int32(now.Nanosecond()/1e6), 8)
	}
}

// RandomNumber sets harvest to a uniform pseudo-random number in [0,1).
func (env *Environment) RandomNumber(harvest *float32) {
	if harvest != nil {
		*harvest = rand.Float32()
	}
}

// RandomSeed reseeds the global PRNG with a random seed.
func (env *Environment) RandomSeed() {
	rand.Seed(time.Now().UnixNano()) //nolint:staticcheck
}
