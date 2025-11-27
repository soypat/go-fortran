package intrinsic

func Int2Bool[T integer](v T) bool {
	return v != 0
}

func Bool2Int[T integer](v bool) T {
	if v {
		return 1
	}
	return 0
}
