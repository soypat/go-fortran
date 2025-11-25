package intrinsic

type Array[T any] struct {
	data  []T
	lower int
}

func (arr Array[T]) Set(i int, v T) {
	arr.data[i-arr.lower] = v
}
