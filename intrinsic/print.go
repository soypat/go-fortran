package intrinsic

import (
	"fmt"
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
	// buf = append(buf, ' ')

	// Format each value
	// - Numeric values: field widths INCLUDE leading separator space
	// - String values: need explicit separator space (except first)
	prevWasString := false
	for i, val := range v {
		// Check if this is a string and not the first item
		_, thisIsString := val.(string)
		dontSpace := i == 1 && prevWasString && thisIsString
		if !dontSpace {
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

	// Determine type-specific field width (Fortran list-directed I/O defaults)
	fieldWidth := f.FieldWidth
	trailingSpace := 0
	if fieldWidth == 0 {
		fieldWidth = 14 // default
	}

	// Format the value based on type
	switch v := value.(type) {
	case string:
		// Strings are printed as-is without field width padding
		dst = append(dst, v...)
		return dst // Skip padding for strings
	case float32: // REAL (single precision)
		// Use 8 decimal places to show full single-precision accuracy
		dst = strconv.AppendFloat(dst, float64(v), 'f', 8, 32)
		fieldWidth = 12 // gfortran empirical: ~18 chars total (3 leading + value + trailing)
		trailingSpace = 4
	case float64: // DOUBLE PRECISION
		// Use 16 decimal places for double precision, correct bit size (64 not 32!)
		dst = strconv.AppendFloat(dst, v, 'f', 16, 64)
		fieldWidth = 25 // gfortran D25.16E3 format (includes leading space)
	case bool: // LOGICAL
		// Fortran prints LOGICAL as T/F, not true/false
		if v {
			dst = append(dst, 'T')
		} else {
			dst = append(dst, 'F')
		}
		fieldWidth = 1 // gfortran L2 format (1 space + T/F)
	case int32: // INTEGER
		dst = strconv.AppendInt(dst, int64(v), 10)
		fieldWidth = 11 // gfortran I12 format (includes leading space)
	}

	// Apply field width padding (right-align) for numeric types
	totalAppended := len(dst) - prevLen
	leftPad := fieldWidth - totalAppended
	if leftPad > 0 {
		// First grow slice if needed
		dst = append(dst, space[:leftPad]...)
		// Now copy value bytes to end (achieves right-alignment)
		copy(dst[len(dst)-totalAppended:], dst[prevLen:prevLen+totalAppended])
		// Now set left pad of bytes to space
		copy(dst[prevLen:prevLen+leftPad], space)
	}
	dst = append(dst, space[:trailingSpace]...)
	return dst
}
