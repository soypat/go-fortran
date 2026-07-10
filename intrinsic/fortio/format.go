package fortio

import (
	"bytes"
	"fmt"
	"math"
	"strconv"
	"strings"

	"github.com/soypat/go-fortran/intrinsic"
)

// Format control characters for compile-time tokenized formats
const (
	FmtNewline byte = '/' // Record separator (newline)
)

// FormatDescriptor represents a single format edit descriptor.
// This type is exported for use by the transpiler to generate pre-parsed format calls.
type FormatDescriptor struct {
	Type      byte   // 'I', 'F', 'E', 'A', 'X', 'S' (string literal), '/' (newline)
	Width     int    // Field width (0 means default)
	Precision int    // Decimal places for F/E formats (-1 means default)
	Literal   string // For string literals (Type='S')
	Repeat    int    // Repeat count (e.g., 6 in 6ES12.4), 0 means 1
}

// Format holds format specification for I/O operations.
type Format struct {
	spec        string             // Raw format specification
	descriptors []FormatDescriptor // Parsed descriptors (lazily populated)
	parsed      bool               // True if spec has been parsed
}

// NewFormat creates a Format from pre-parsed format components.
// Arguments can be:
//   - FormatDescriptor: a format descriptor struct
//   - string: a string literal (shorthand for FormatDescriptor{Type:'S', Literal:s})
//   - byte: a control character (e.g., FmtNewline for '/')
//
// For backward compatibility, a single string argument is treated as a raw format spec
// to be parsed at runtime (deprecated - new code should use pre-parsed components).
func NewFormat(args ...any) *Format {
	// Backward compatibility: single string argument = raw format spec
	if len(args) == 1 {
		if spec, ok := args[0].(string); ok {
			return &Format{spec: spec}
		}
	}

	// Pre-parsed format components
	f := &Format{parsed: true}
	for _, arg := range args {
		switch v := arg.(type) {
		case FormatDescriptor:
			// Handle repeat count by duplicating descriptor
			repeat := v.Repeat
			if repeat <= 0 {
				repeat = 1
			}
			for i := 0; i < repeat; i++ {
				f.descriptors = append(f.descriptors, FormatDescriptor{
					Type:      v.Type,
					Width:     v.Width,
					Precision: v.Precision,
					Literal:   v.Literal,
				})
			}
		case string:
			f.descriptors = append(f.descriptors, FormatDescriptor{Type: 'S', Literal: v})
		case byte:
			f.descriptors = append(f.descriptors, FormatDescriptor{Type: v})
		}
	}
	return f
}

// DefaultFormat returns a default (list-directed) format.
func DefaultFormat() *Format {
	return &defaultFormat
}

// FormatFromCharVar creates a Format from a Fortran CHARACTER variable or array.
// Accepts intrinsic.CharacterArray (scalar) or *intrinsic.Array[intrinsic.CharacterArray] (array).
func FormatFromCharVar(v any) *Format {
	switch fv := v.(type) {
	case intrinsic.CharacterArray:
		return NewFormat(fv.String())
	case *intrinsic.Array[intrinsic.CharacterArray]:
		lo := fv.LowerDim(1)
		hi := fv.UpperDim(1)
		var sb strings.Builder
		sb.Grow(hi - lo + 1)
		for i := lo; i <= hi; i++ {
			sb.WriteString(fv.At(i).String())
		}
		return NewFormat(sb.String())
	}
	return DefaultFormat()
}

var defaultFormat Format

// ensureParsed parses the format spec if not already parsed.
func (f *Format) ensureParsed() {
	if f.parsed {
		return
	}
	f.parsed = true
	f.descriptors = parseFormatSpec(f.spec)
}

// parseFormatSpec parses a format specification string into descriptors.
// Example: "'m=', I3,' n=', I3,' x=', F5.2" produces:
// [{Type:'S', Literal:"m="}, {Type:'I', Width:3}, {Type:'S', Literal:" n="}, ...]
func parseFormatSpec(spec string) []FormatDescriptor {
	var descriptors []FormatDescriptor
	i := 0
	for i < len(spec) {
		// Skip whitespace and commas
		for i < len(spec) && (spec[i] == ' ' || spec[i] == ',' || spec[i] == '\t') {
			i++
		}
		if i >= len(spec) {
			break
		}

		// String literal: 'text'
		if spec[i] == '\'' {
			i++ // skip opening quote
			start := i
			for i < len(spec) && spec[i] != '\'' {
				i++
			}
			descriptors = append(descriptors, FormatDescriptor{
				Type:    'S',
				Literal: spec[start:i],
			})
			if i < len(spec) {
				i++ // skip closing quote
			}
			continue
		}

		// Format descriptor: I3, F5.2, A, X, etc.
		if spec[i] >= 'A' && spec[i] <= 'Z' || spec[i] >= 'a' && spec[i] <= 'z' {
			typ := spec[i]
			if typ >= 'a' && typ <= 'z' {
				typ -= 32 // uppercase
			}
			i++

			// Parse width
			width := 0
			for i < len(spec) && spec[i] >= '0' && spec[i] <= '9' {
				width = width*10 + int(spec[i]-'0')
				i++
			}

			// Parse precision (for F, E formats)
			precision := -1
			if i < len(spec) && spec[i] == '.' {
				i++ // skip '.'
				precision = 0
				for i < len(spec) && spec[i] >= '0' && spec[i] <= '9' {
					precision = precision*10 + int(spec[i]-'0')
					i++
				}
			}

			descriptors = append(descriptors, FormatDescriptor{
				Type:      typ,
				Width:     width,
				Precision: precision,
			})
			continue
		}

		// Skip unknown character
		i++
	}
	return descriptors
}

// writeFormatted applies format descriptors to arguments.
func (f *Format) writeFormatted(buf []byte, args []any) []byte {
	argIdx := 0
	for _, desc := range f.descriptors {
		switch desc.Type {
		case 'S': // String literal
			buf = append(buf, desc.Literal...)
		case 'I': // Integer
			if argIdx < len(args) {
				buf = formatInt(buf, args[argIdx], desc.Width)
				argIdx++
			}
		case 'F': // Float (fixed-point)
			if argIdx < len(args) {
				buf = formatFloat(buf, args[argIdx], desc.Width, desc.Precision)
				argIdx++
			}
		case 'A': // Character/string
			if argIdx < len(args) {
				buf = formatString(buf, args[argIdx], desc.Width)
				argIdx++
			}
		case 'X': // Skip spaces
			for i := 0; i < desc.Width; i++ {
				buf = append(buf, ' ')
			}
		case '/': // Newline/record separator
			buf = append(buf, '\n')
		case 'E': // Exponential format (scientific notation)
			if argIdx < len(args) {
				buf = formatExponential(buf, args[argIdx], desc.Width, desc.Precision)
				argIdx++
			}
		}
	}
	return buf
}

// writeListDirected writes values in list-directed format (PRINT * behavior).
func (f *Format) writeListDirected(buf []byte, args []any) []byte {
	// Fortran PRINT * adds leading space (carriage control character)
	buf = append(buf, ' ')

	// Format each value
	prevWasString := false
	for i, val := range args {
		_, thisIsString := val.(string)
		isStringType := thisIsString || isCharacterArray(val)
		dontSpace := prevWasString && isStringType
		if !dontSpace && i > 0 {
			buf = append(buf, ' ')
		}
		buf = formatValue(buf, val)
		prevWasString = isStringType
	}
	return buf
}

// formatInt formats an integer with specified width (right-aligned).
func formatInt(buf []byte, val any, width int) []byte {
	var n int64
	switch v := val.(type) {
	case int:
		n = int64(v)
	case int8:
		n = int64(v)
	case int16:
		n = int64(v)
	case int32:
		n = int64(v)
	case int64:
		n = v
	default:
		return buf
	}
	startLen := len(buf)
	buf = strconv.AppendInt(buf, n, 10)
	numLen := len(buf) - startLen
	// Right-align with spaces
	if pad := width - numLen; pad > 0 {
		buf = padLeft(buf, startLen, pad)
	}
	return buf
}

// formatFloat formats a float with specified width and precision.
func formatFloat(buf []byte, val any, width int, precision int) []byte {
	var x float64
	switch v := val.(type) {
	case float32:
		x = float64(v)
	case float64:
		x = v
	default:
		return buf
	}
	if precision < 0 {
		precision = 2 // default
	}
	startLen := len(buf)
	buf = strconv.AppendFloat(buf, x, 'f', precision, 64)
	numLen := len(buf) - startLen
	// Right-align with spaces
	if pad := width - numLen; pad > 0 {
		buf = padLeft(buf, startLen, pad)
	}
	return buf
}

// formatExponential formats a float in exponential/scientific notation (E format).
func formatExponential(buf []byte, val any, width int, precision int) []byte {
	var x float64
	switch v := val.(type) {
	case float32:
		x = float64(v)
	case float64:
		x = v
	default:
		return buf
	}
	if precision < 0 {
		precision = 4 // default for E format
	}
	startLen := len(buf)
	buf = strconv.AppendFloat(buf, x, 'E', precision, 64)
	numLen := len(buf) - startLen
	// Right-align with spaces
	if pad := width - numLen; pad > 0 {
		buf = padLeft(buf, startLen, pad)
	}
	return buf
}

// formatString formats a string with specified width.
func formatString(buf []byte, val any, width int) []byte {
	var s string
	switch v := val.(type) {
	case string:
		s = v
	case fmt.Stringer:
		s = v.String()
	default:
		return buf
	}
	buf = append(buf, s...)
	// Pad with spaces if width specified
	for i := len(s); i < width; i++ {
		buf = append(buf, ' ')
	}
	return buf
}

// splitFields splits a line into whitespace/comma-separated fields.
func splitFields(s string) []string {
	var fields []string
	var current bytes.Buffer
	for _, r := range s {
		if r == ' ' || r == ',' || r == '\t' {
			if current.Len() > 0 {
				fields = append(fields, current.String())
				current.Reset()
			}
		} else {
			current.WriteRune(r)
		}
	}
	if current.Len() > 0 {
		fields = append(fields, current.String())
	}
	return fields
}

// trimSpaces removes leading and trailing spaces.
func trimSpaces(s string) string {
	start := 0
	for start < len(s) && s[start] == ' ' {
		start++
	}
	end := len(s)
	for end > start && s[end-1] == ' ' {
		end--
	}
	return s[start:end]
}

// padLeft inserts padding spaces before the value at startOff.
func padLeft(dst []byte, startOff, leftPad int) []byte {
	const space = "                                         "
	strLen := len(dst) - startOff
	// First grow slice if needed
	dst = append(dst, space[:leftPad]...)
	// Now copy value bytes to end (achieves right-alignment)
	copy(dst[len(dst)-strLen:], dst[startOff:startOff+strLen])
	// Now set left pad of bytes to space
	copy(dst[startOff:startOff+leftPad], space)
	return dst
}

// fixExponent pads exponent to 3 digits minimum per F95 spec (E-005 not E-05)
func fixExponent(dst []byte, start int) []byte {
	if i := bytes.IndexByte(dst[start:], 'E'); i >= 0 {
		i += start
		if i+2 < len(dst) && (dst[i+1] == '+' || dst[i+1] == '-') {
			// Find where exponent digits start
			expStart := i + 2
			expDigits := dst[expStart:]
			// Pad to 3 digits
			for len(expDigits) < 3 {
				dst = append(dst[:expStart], append([]byte{'0'}, dst[expStart:]...)...)
				expDigits = dst[expStart:]
			}
		}
	}
	return dst
}

// isCharacterArray checks if val is an intrinsic.CharacterArray.
func isCharacterArray(val any) bool {
	if _, ok := val.(intrinsic.CharacterArray); ok {
		return true
	}
	_, ok := val.(*intrinsic.CharacterArray)
	return ok
}

func isString(val any) bool {
	_, ok := val.(string)
	return ok
}

// formatValue formats a value for list-directed output (PRINT * behavior).
func formatValue(dst []byte, value any) []byte {
	prevLen := len(dst)

	// Control variables: leftPad and rightPad calculated per type
	var leftPad, rightPad int

	// Format value and determine padding (from gfortran libgfortran/io/write.c)
	switch v := value.(type) {
	case intrinsic.CharacterArray:
		dst = append(dst, v.String()...)
		return dst
	case *intrinsic.CharacterArray:
		dst = append(dst, v.String()...)
		return dst
	case string:
		dst = append(dst, v...)
		return dst

	case int8: // INTEGER (kind=1): 1 leading space (+ 1 from Print = 2 total)
		dst = strconv.AppendInt(dst, int64(v), 10)
		leftPad = 1
		rightPad = 0

	case int16: // INTEGER (kind=2): 1 leading space (+ 1 from Print = 2 total)
		dst = strconv.AppendInt(dst, int64(v), 10)
		leftPad = 1
		rightPad = 0

	case int32: // INTEGER (kind=4): width=11, right-aligned
		dst = strconv.AppendInt(dst, int64(v), 10)
		leftPad = 11 - (len(dst) - prevLen)
		rightPad = 0
	case int:
		dst = strconv.AppendInt(dst, int64(v), 10)
		leftPad = 11 - (len(dst) - prevLen)
		rightPad = 0

	case int64: // INTEGER (kind=8): 1 leading space (+ 1 from Print = 2 total)
		dst = strconv.AppendInt(dst, v, 10)
		leftPad = 1
		rightPad = 0

	case float32, float64: // REAL: gfortran formatting
		var width, totalWidth int
		var x float64
		if k, ok := v.(float32); ok {
			x = float64(k)
			width = 10
			totalWidth = 16
		} else if k, ok := v.(float64); ok {
			x = k
			width = 18
			totalWidth = 25
		}
		absX := math.Abs(x)
		usedEFormat := false
		// gfortran uses exponential for very small values
		if absX > 0 && absX < 0.01 {
			dst = strconv.AppendFloat(dst, x, 'E', 16, 64)
			// F95 requires min 3-digit exponent (E-005 not E-05)
			dst = fixExponent(dst, prevLen)
			usedEFormat = true
		} else {
			// Fixed format with variable precision based on magnitude
			var decPlaces int
			if absX >= 1.0 || absX == 0.0 {
				nIntDig := 1
				if absX >= 1.0 {
					nIntDig = int(math.Log10(absX)) + 1
				}
				decPlaces = width - nIntDig - 1
			} else {
				if width == 18 { // DOUBLE PRECISION
					decPlaces = width - 2 // "0." takes 2 chars for float64
				} else {
					decPlaces = width - 1 // float32 uses different formula
				}
			}
			dst = strconv.AppendFloat(dst, x, 'f', decPlaces, 64)
		}
		valueLen := len(dst) - prevLen // Calculate AFTER fixExponent
		if usedEFormat {
			// E format: use full width minus value (no separate left/right distribution)
			leftPad = totalWidth - valueLen
			rightPad = 0
		} else if absX < 1.0 && absX != 0.0 {
			if width == 18 { // DOUBLE PRECISION needs more left padding
				leftPad = 2
			} else {
				leftPad = 1
			}
			rightPad = totalWidth - leftPad - valueLen
		} else {
			leftPad = 2
			rightPad = totalWidth - leftPad - valueLen
		}

	case bool: // LOGICAL: just T or F, no padding
		if v {
			dst = append(dst, 'T')
		} else {
			dst = append(dst, 'F')
		}
		return dst // No padding
	default:
		panic(fmt.Sprintf("unsupported format type: %T", value))
	}
	const space = "                                         "
	// Apply padding
	if leftPad > 0 {
		dst = padLeft(dst, prevLen, leftPad)
	}
	if rightPad > 0 {
		dst = append(dst, space[:rightPad]...)
	}
	return dst
}

// scanValue parses a string into a pointer variable.
func scanValue(field string, arg any) error {
	switch p := arg.(type) {
	case *int32:
		n, err := strconv.ParseInt(field, 10, 32)
		if err != nil {
			return err
		}
		*p = int32(n)
	case *int64:
		n, err := strconv.ParseInt(field, 10, 64)
		if err != nil {
			return err
		}
		*p = n
	case *float32:
		f, err := strconv.ParseFloat(field, 32)
		if err != nil {
			return err
		}
		*p = float32(f)
	case *float64:
		f, err := strconv.ParseFloat(field, 64)
		if err != nil {
			return err
		}
		*p = f
	case *string:
		*p = field
	case *intrinsic.CharacterArray:
		p.SetFromString(field)
	}
	return nil
}

// readListDirected parses space/comma-separated values.
func readListDirected(line string, args []any) error {
	fields := splitFields(line)
	for i, arg := range args {
		if i >= len(fields) {
			break
		}
		if err := scanValue(fields[i], arg); err != nil {
			return err
		}
	}
	return nil
}

// readFormatted parses data according to format descriptors.
func readFormatted(line string, descriptors []FormatDescriptor, args []any) error {
	pos := 0
	argIdx := 0
	for _, desc := range descriptors {
		if argIdx >= len(args) {
			break
		}
		switch desc.Type {
		case 'I': // Integer
			width := desc.Width
			if width == 0 {
				width = 10 // default
			}
			if pos+width > len(line) {
				width = len(line) - pos
			}
			field := trimSpaces(line[pos : pos+width])
			pos += width
			if err := scanValue(field, args[argIdx]); err != nil {
				return err
			}
			argIdx++
		case 'F', 'E', 'G', 'D': // Float formats
			width := desc.Width
			if width == 0 {
				width = 15 // default
			}
			if pos+width > len(line) {
				width = len(line) - pos
			}
			field := trimSpaces(line[pos : pos+width])
			pos += width
			if err := scanValue(field, args[argIdx]); err != nil {
				return err
			}
			argIdx++
		case 'A': // Character
			width := desc.Width
			if width == 0 {
				width = len(line) - pos // rest of line
			}
			if pos+width > len(line) {
				width = len(line) - pos
			}
			field := line[pos : pos+width]
			pos += width
			if err := scanValue(field, args[argIdx]); err != nil {
				return err
			}
			argIdx++
		case 'X': // Skip
			pos += desc.Width
		case 'S', '/':
			// Literals and newlines don't consume args
		}
	}
	return nil
}
