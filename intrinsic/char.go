package intrinsic

type CharacterArray struct {
	data []byte
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
