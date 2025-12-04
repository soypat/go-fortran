package intrinsic

import (
	"os"
	"strconv"
)

func Stop(code int) {
	buf := "STOP " + strconv.Itoa(code)
	os.Stdout.WriteString(buf)
	os.Stdout.Sync()
	os.Exit(code)
}
