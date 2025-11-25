package intrinsic

import "strconv"

type Formatter struct {
	FieldWidth int
}

func (f Formatter) Print(v ...any) {

}

func (f Formatter) formatValue(dst []byte, value any) []byte {
	const space = "                                         "
	prevLen := len(dst)
	switch v := value.(type) {
	case string:
		dst = append(dst, v...)
	case float32: // REAL
		dst = strconv.AppendFloat(dst, float64(v), 'f', 5, 32)
	case float64: // REAL
		dst = strconv.AppendFloat(dst, v, 'f', 5, 32)
	case bool: // BOOL
		if v {
			dst = append(dst, 'T')
		} else {
			dst = append(dst, 'F')
		}
	case int32: // INTEGER
		dst = strconv.AppendInt(dst, int64(v), 10)
	}
	totalAppended := len(dst) - prevLen
	leftPad := f.FieldWidth - totalAppended
	if leftPad > 0 {
		// first grow slice if needed.
		dst = append(dst, space[:leftPad]...)
		// now copy value bytes to end.
		copy(dst[len(dst)-totalAppended:], dst[prevLen:prevLen+totalAppended])
		// Now set left pad of bytes to space.
		copy(dst[prevLen:prevLen+leftPad], space)
	}
	return dst
}
