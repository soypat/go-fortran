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
	buf = append(buf, ' ')

	// Format each value with spacing between items
	for i, val := range v {
		if i > 0 {
			// Fortran adds space between list items
			buf = append(buf, ' ')
		}
		buf = f.formatValue(buf, val)
	}

	// Print with newline (Fortran PRINT statement behavior)
	fmt.Println(string(buf))
}

func (f Formatter) formatValue(dst []byte, value any) []byte {
	const space = "                                         "
	prevLen := len(dst)

	// Determine type-specific field width (Fortran list-directed I/O defaults)
	fieldWidth := f.FieldWidth
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
		fieldWidth = 14 // Fortran REAL default field width
	case float64: // DOUBLE PRECISION
		// Use 16 decimal places for double precision, correct bit size (64 not 32!)
		dst = strconv.AppendFloat(dst, v, 'f', 16, 64)
		fieldWidth = 24 // Fortran DOUBLE PRECISION default field width
	case bool: // LOGICAL
		// Fortran prints LOGICAL as T/F, not true/false
		if v {
			dst = append(dst, 'T')
		} else {
			dst = append(dst, 'F')
		}
		fieldWidth = 2 // Fortran LOGICAL default field width (space + T/F)
	case int32: // INTEGER
		dst = strconv.AppendInt(dst, int64(v), 10)
		fieldWidth = 11 // Fortran INTEGER default field width (gfortran)
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
	return dst
}
