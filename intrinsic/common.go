package intrinsic

import (
	"fmt"
	"unsafe"
)

func NewCommonBlock(name string, size int) CommonBlock {
	return CommonBlock{
		name: name,
		data: make([]byte, size),
	}
}

type CommonBlock struct {
	name   string
	data   []byte
	offset int
}

func (cb *CommonBlock) Reset() {
	cb.offset = 0
}

func DeclareCommon(set PointerSetter, cb *CommonBlock) {
	elemsz := set.SizeElement()
	if cb.offset%elemsz != 0 {
		panic(fmt.Sprintf("common %s declaration failed on alignment : element size %d with offset %d", cb.name, elemsz, cb.offset))
	}
	sz := set.LenBuffer() * elemsz
	if cb.offset+sz > len(cb.data) {
		panic(fmt.Sprintf("common %s declaration failed on overflow: buffer size %d and common has %d/%d", cb.name, sz, len(cb.data)-cb.offset, len(cb.data)))
	}
	set.SetDataUnsafe(unsafe.Pointer(&cb.data[cb.offset]))

	cb.offset += sz
}

// type CommonPointer[T any] struct {
// 	dataStart []byte
// }
// var hostOrder = binary.LittleEndian
// func (cp CommonPointer[T]) Uint64() uint64   { return hostOrder.Uint64(cp.dataStart) }
// func (cp CommonPointer[T]) Uint32() uint32   { return hostOrder.Uint32(cp.dataStart) }
// func (cp CommonPointer[T]) Uint16() uint16   { return hostOrder.Uint16(cp.dataStart) }
// func (cp CommonPointer[T]) Uint8() byte      { return cp.dataStart[0] }
// func (cp CommonPointer[T]) Int64() int64     { return int64(cp.Uint64()) }
// func (cp CommonPointer[T]) Int32() int32     { return int32(cp.Uint32()) }
// func (cp CommonPointer[T]) Int16() int16     { return int16(cp.Uint16()) }
// func (cp CommonPointer[T]) Int8() int8       { return int8(cp.Uint8()) }
// func (cp CommonPointer[T]) Float64() float64 { return math.Float64frombits(cp.Uint64()) }
// func (cp CommonPointer[T]) Float32() float32 { return math.Float32frombits(cp.Uint32()) }
