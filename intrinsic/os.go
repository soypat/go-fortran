package intrinsic

import "os"

func Exit(code int) {
	os.Stdout.Sync()
	os.Exit(code)
}
