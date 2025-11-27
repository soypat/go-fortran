package intrinsic

import (
	"fmt"
	"math"
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
		dontSpace := i == 1 && prevWasString && isStringType
		if !dontSpace && i > 0 { // Don't add separator before first item
			buf = append(buf, ' ')
		}
		buf = f.formatValue(buf, val)
		prevWasString = isStringType
	}

	// Print with newline (Fortran PRINT statement behavior)
	fmt.Println(string(buf))
}

func (f Formatter) formatValue(dst []byte, value any) []byte {
	const space = "                                         "
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

	case float32, float64: // REAL (kind=4): gfortran uses different widths based on magnitude
		// gfortran uses: 11 chars (9 decimals) for |x| < 1, 10 chars (variable) for |x| >= 1
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
		var decPlaces int
		absX := math.Abs(x)
		if absX < 1.0 {
			// Values like 0.479... get 11 chars total: 0 + '.' + 9 decimals
			decPlaces = width - 1
		} else {
			// Values like 3.14... get 10 chars total with variable decimals
			nIntDig := int(math.Log10(absX)) + 1
			decPlaces = width - nIntDig - 1 // e.g., 10 - 1 - 1 = 8 for single-digit values
		}
		dst = strconv.AppendFloat(dst, x, 'f', decPlaces, 64)
		valueLen := len(dst) - prevLen
		// Total field width = 17 (includes 1 separator added by Print)
		// formatValue adds: leftPad + value + rightPad = 16
		// Pattern: |x| < 1 → 2 leading (1 sep + 1 pad) + 11-char value + 4 trailing
		//          |x| >= 1 → 3 leading (1 sep + 2 pad) + 10-char value + 4 trailing
		if absX < 1.0 {
			leftPad = 1 // 1 sep (Print) + 1 leftPad = 2 total leading spaces
		} else {
			leftPad = 2 // 1 sep (Print) + 2 leftPad = 3 total leading spaces
		}
		rightPad = totalWidth - leftPad - valueLen

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

	// Apply padding
	if leftPad > 0 {
		totalAppended := len(dst) - prevLen
		// First grow slice if needed
		dst = append(dst, space[:leftPad]...)
		// Now copy value bytes to end (achieves right-alignment)
		copy(dst[len(dst)-totalAppended:], dst[prevLen:prevLen+totalAppended])
		// Now set left pad of bytes to space
		copy(dst[prevLen:prevLen+leftPad], space)
	}
	if rightPad > 0 {
		dst = append(dst, space[:rightPad]...)
	}
	return dst
}
