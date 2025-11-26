package intrinsic

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
	n := copy(ch.data[:cap(ch.data)], data)
	ch.data = ch.data[:n]
	ch.setUnusedToSpace()
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

func (ch *CharacterArray) setUnusedToSpace() {
	raw := ch.data[len(ch.data):cap(ch.data)]
	for i := range raw {
		raw[i] = ' '
	}
}
