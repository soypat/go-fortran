package intrinsic

import (
	"bytes"
	"unsafe"
)

// CharacterArray represents a Fortran CHARACTER(LEN=n) variable with fixed length.
//
// Design: Uses Go slice len/cap duality for metadata:
//   - cap(data): Fortran declared length (fixed, immutable)
//   - len(data): Actual data length (user extension, not part of Fortran semantics)
//
// To match Fortran semantics, always use cap(data) for the effective length.
// Methods automatically handle space padding and truncation to cap(data).
//
// Example:
//
//	// Fortran: CHARACTER(LEN=20) :: str
//	str := NewCharacterArray(20)  // cap=20, len=0 initially
//	str.SetFromString("Hello")     // Sets "Hello" + 15 spaces, len=5
//	s := str.String()              // Returns full 20-char string with padding
type CharacterArray struct {
	data []byte // Slice where cap(data) = Fortran LEN, len(data) = actual bytes set
}

func NewCharacterArray(len int) CharacterArray {
	ch := CharacterArray{
		data: make([]byte, 0, len),
	}
	ch.setUnusedToSpace()
	return ch
}

var _ pointer = CharacterArray{}

// DataUnsafe implements [pointer] interface.
func (ch CharacterArray) DataUnsafe() unsafe.Pointer {
	return unsafe.Pointer(&ch.data[0])
}

// SizeElement returns the number of bytes per character. Always returns 1 in Go. Implements [pointer] interface.
func (ch CharacterArray) SizeElement() int {
	return 1
}

// LenBuffer returns length of flattened character buffer in characters (bytes). Implements [pointer] interface.
func (ch CharacterArray) LenBuffer() int {
	return cap(ch.data)
}

func (ch CharacterArray) At(i int) byte {
	return ch.data[i-1]
}

func (ch CharacterArray) Set(i int, v byte) {
	ch.data[i-1] = v
}

func (ch CharacterArray) StringLen() string {
	return string(ch.data)
}

func (ch CharacterArray) String() string {
	return string(ch.data[:cap(ch.data)])
}

func (ch *CharacterArray) SetFromString(data string) {
	ch.data = ch.data[:cap(ch.data)] // Ensure full capacity for modification
	n := copy(ch.data, data)
	// Pad the rest with spaces
	for i := n; i < cap(ch.data); i++ {
		ch.data[i] = ' '
	}
	// Keep data at full capacity (Fortran semantics)
}

func (ch *CharacterArray) SetConcat(toJoin ...CharacterArray) {
	ch.data = ch.data[:cap(ch.data)]
	off := 0
	for i := range toJoin {
		off += copy(ch.data[off:], toJoin[i].data)
		if off >= len(ch.data) {
			break
		}
	}
	ch.data = ch.data[:off]
	ch.setUnusedToSpace()
}

func (ch *CharacterArray) SetConcatString(toJoin ...string) {
	ch.data = ch.data[:cap(ch.data)]
	off := 0
	for i := range toJoin {
		off += copy(ch.data[off:], toJoin[i])
		if off >= len(ch.data) {
			break
		}
	}
	ch.data = ch.data[:off]
	ch.setUnusedToSpace()
}

func (ch *CharacterArray) setUnusedToSpace() {
	raw := ch.data[len(ch.data):cap(ch.data)]
	for i := range raw {
		raw[i] = ' '
	}
}

// Len returns the declared length of the CHARACTER variable (LEN intrinsic)
// Corresponds to Fortran: LEN(str)
func (ch CharacterArray) Len() int {
	return cap(ch.data)
}

// LenTrim returns the length without trailing spaces (LEN_TRIM intrinsic)
// Corresponds to Fortran: LEN_TRIM(str)
func (ch CharacterArray) LenTrim() int {
	s := ch.data[:cap(ch.data)]
	// Find last non-space character
	for i := len(s) - 1; i >= 0; i-- {
		if s[i] != ' ' {
			return i + 1
		}
	}
	return 0 // All spaces
}

// Trim returns a new CharacterArray with trailing spaces removed (TRIM intrinsic)
// The result has the same declared length but different content
// Corresponds to Fortran: TRIM(str)
func (ch CharacterArray) Trim() CharacterArray {
	lenTrim := ch.LenTrim()
	result := NewCharacterArray(cap(ch.data))
	copy(result.data[:cap(result.data)], ch.data[:lenTrim])
	// Rest is already spaces from NewCharacterArray
	return result
}

// Index returns the 1-based starting position of substring in string (INDEX intrinsic)
// Returns 0 if not found
// Corresponds to Fortran: INDEX(str, substring)
func (ch CharacterArray) Index(substring string) int {
	return 1 + bytes.Index(ch.data, unsafe.Slice(unsafe.StringData(substring), len(substring)))
}

// AdjustL returns a new CharacterArray with leading spaces moved to the end (ADJUSTL intrinsic)
// Corresponds to Fortran: ADJUSTL(str)
func (ch CharacterArray) AdjustL() CharacterArray {
	s := ch.data[:cap(ch.data)]
	result := NewCharacterArray(cap(ch.data))

	// Find first non-space character
	firstNonSpace := 0
	for i, c := range s {
		if c != ' ' {
			firstNonSpace = i
			break
		}
	}

	// Copy non-leading-space portion to start of result
	copy(result.data[:cap(result.data)], s[firstNonSpace:])
	// Rest is already spaces from NewCharacterArray
	return result
}

// AdjustR returns a new CharacterArray with trailing spaces moved to the start (ADJUSTR intrinsic)
// Corresponds to Fortran: ADJUSTR(str)
func (ch CharacterArray) AdjustR() CharacterArray {
	lenTrim := ch.LenTrim()
	result := NewCharacterArray(cap(ch.data))

	// Copy trimmed content to end of result
	offset := cap(ch.data) - lenTrim
	copy(result.data[offset:cap(result.data)], ch.data[:lenTrim])
	// Leading portion is already spaces from NewCharacterArray
	return result
}

// Substring returns a copy of the string from start to end.
// Corresponds to Fortran: str(start:end)
func (ch CharacterArray) Substring(start, end int) string {
	return ch.View(start, end).String()
}

// View returns a view into the substring from start to end (1-based, inclusive)
func (ch CharacterArray) View(start, end int) CharacterArray {
	if end > cap(ch.data) {
		end = cap(ch.data)
	}
	if start > end {
		return CharacterArray{}
	}
	return CharacterArray{
		// Convert start to 0-based indexing and limit capacity to match length
		// Using 3-index slice: data[low:high:max] where max sets the capacity
		data: ch.data[start-1 : end : end],
	}
}
