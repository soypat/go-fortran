package intrinsic

import (
	"bytes"
	"fmt"
	"math"
	"os"
	"strconv"
)

var defaultFormatter Formatter

func Print(v ...any) {
	defaultFormatter.Print(v...)
}

type Formatter struct {
}

// Print formats and prints values with Fortran list-directed I/O formatting
func (f Formatter) Print(v ...any) {
	var buf []byte

	// Fortran PRINT * adds leading space (carriage control character)
	buf = append(buf, ' ')

	// Format each value
	// - Numeric values: field widths INCLUDE leading separator space
	// - String values: need explicit separator space (except first)
	prevWasString := false
	for i, val := range v {
		// Check if this is a string (or CharacterArray) and not the first item
		_, thisIsString := val.(string)
		_, thisIsCharArray := val.(CharacterArray)
		isStringType := thisIsString || thisIsCharArray
		dontSpace := prevWasString && isStringType
		if !dontSpace && i > 0 { // Don't add separator before first item
			buf = append(buf, ' ')
		}
		buf = f.formatValue(buf, val)
		prevWasString = isStringType
	}
	buf = append(buf, '\n')
	// Print with newline (Fortran PRINT statement behavior)
	os.Stdout.Write(buf)
}

func (f Formatter) formatValue(dst []byte, value any) []byte {

	prevLen := len(dst)

	// Control variables: leftPad and rightPad calculated per type
	var leftPad, rightPad int

	// Format value and determine padding (from gfortran libgfortran/io/write.c)
	switch v := value.(type) {
	case CharacterArray:
		dst = append(dst, v.data[:cap(v.data)]...)
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
			if absX < 1.0 {
				if width == 18 { // DOUBLE PRECISION
					decPlaces = width - 2 // "0." takes 2 chars for float64
				} else {
					decPlaces = width - 1 // float32 uses different formula
				}
			} else {
				nIntDig := int(math.Log10(absX)) + 1
				decPlaces = width - nIntDig - 1
			}
			dst = strconv.AppendFloat(dst, x, 'f', decPlaces, 64)
		}
		valueLen := len(dst) - prevLen // Calculate AFTER fixExponent
		if usedEFormat {
			// E format: use full width minus value (no separate left/right distribution)
			leftPad = totalWidth - valueLen
			rightPad = 0
		} else if absX < 1.0 {
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

// appendFloat formats floating-point per F95 list-directed output (10.8.2):
// Uses F format if magnitude in range, else E format with 2-digit exponent minimum
func appendFloat[T float](dst []byte, x T, fmt byte, prec int) []byte {
	s := strconv.AppendFloat(dst, float64(x), fmt, prec, 64)
	// F95 requires min 2-digit exponent (E-005 not E-05)
	if i := bytes.IndexByte(s[len(dst):], 'E'); i >= 0 {
		i += len(dst)
		// Find exponent sign
		if i+1 < len(s) && (s[i+1] == '+' || s[i+1] == '-') {
			exp := s[i+2:]
			// Pad to 3 digits if needed (Fortran E format minimum)
			if len(exp) < 3 {
				s = append(s[:i+2], '0')
				s = append(s, exp...)
			}
			if len(exp) < 2 {
				s = append(s[:i+2], '0')
				s = append(s, s[i+2:]...)
			}
		}
	}
	return s
}

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
