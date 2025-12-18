package fortio

import (
	"io"
	"os"
	"strconv"
)

// unitState tracks connection info for each open unit.
type unitState struct {
	file     *os.File
	name     string
	access   AccessMode
	form     FormMode
	action   ActionMode
	position PositionMode
	recl     int32
	blank    BlankMode
	delim    DelimMode
	pad      PadMode
	decimal  DecimalMode
	round    RoundMode
	sign     SignMode
	encoding EncodingMode
	async    AsyncMode
	scratch  bool  // is scratch file
	pos      int64 // current byte position (stream)
	rec      int32 // current record number (direct)
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
}

// NewEnvironment creates a new Fortran runtime environment
// with standard units pre-connected.
func NewEnvironment() *Environment {
	env := &Environment{
		units: make(map[int32]*unitState),
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

	f, err := os.OpenFile(spec.FILE, flag, 0644)
	if err != nil {
		stat := env.mapError(err)
		if spec.IOSTAT != nil {
			*spec.IOSTAT = stat
		}
		if spec.IOMSG != nil {
			*spec.IOMSG = err.Error()
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
		*spec.IOSTAT = IOStatOK
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
			*spec.IOSTAT = IOStatOK
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
			*spec.IOSTAT = stat
		}
		if spec.IOMSG != nil {
			*spec.IOMSG = err.Error()
		}
		return stat
	}

	if spec.IOSTAT != nil {
		*spec.IOSTAT = IOStatOK
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

// Read performs formatted input from a unit.
func (env *Environment) Read(unit int32, f *Format, args ...any) IOStat {
	state := env.getUnit(unit)
	if state == nil {
		return IOStatErrNotConnected
	}
	if state.file == nil {
		return IOStatErrNotConnected
	}

	// Read a line from the unit
	var line []byte
	oneByte := make([]byte, 1)
	for {
		n, err := state.file.Read(oneByte)
		if n > 0 {
			if oneByte[0] == '\n' {
				break
			}
			line = append(line, oneByte[0])
		}
		if err != nil {
			if err == io.EOF && len(line) > 0 {
				break
			}
			stat := env.mapError(err)
			env.lastStat = stat
			env.lastMsg = err.Error()
			return stat
		}
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
