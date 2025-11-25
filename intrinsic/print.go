package intrinsic

import (
	"fmt"
	"math"
	"strconv"
)

type Formatter struct {
	FieldWidth int
}

// NewFormatter creates a Formatter with Fortran-compatible default field width
func NewFormatter() Formatter {
	return Formatter{FieldWidth: 14} // Typical Fortran list-directed I/O field width
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
		// Check if this is a string and not the first item
		_, thisIsString := val.(string)
		dontSpace := i == 1 && prevWasString && thisIsString
		if !dontSpace && i > 0 { // Don't add separator before first item
			buf = append(buf, ' ')
		}
		buf = f.formatValue(buf, val)
		prevWasString = thisIsString
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
	case string:
		dst = append(dst, v...)
		return dst

	case int32: // INTEGER (kind=4): width=11, right-aligned
		dst = strconv.AppendInt(dst, int64(v), 10)
		leftPad = 11 - (len(dst) - prevLen)
		rightPad = 0

	case float32: // REAL (kind=4): width=18, 2 left + value + right
		// gfortran uses variable precision to keep value at 10 chars total
		x := float64(v)
		dst = strconv.AppendFloat(dst, x, 'f', 8, 32)
		nIntDig := int(math.Log10(math.Abs(x))) + 1
		leftPad = 3 - nIntDig
		rightPad = 16 - (len(dst) - prevLen) - leftPad

	case float64: // DOUBLE PRECISION (kind=8): width=25, right-aligned
		dst = strconv.AppendFloat(dst, v, 'f', 16, 64)
		nIntDig := int(math.Log10(math.Abs(v))) + 1
		leftPad = 3 - nIntDig
		rightPad = 16 - (len(dst) - prevLen) - leftPad

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
