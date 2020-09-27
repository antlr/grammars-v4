package samples

import (
	"fmt"
	"runtime"
)

func f() int {
	return 1
}

func g() {}

func SwitchesExpr() int {
	switch os := runtime.GOOS; os {
	case "darwin":
		fmt.Println("OS X.")
	case "linux":
		fmt.Println("Linux.")
	default:
		// freebsd, openbsd,
		// plan9, windows...
		fmt.Printf("%s.\n", os)
	}

	tag := 1
	switch tag {
		default: g()
		case 0, 1, 2, 3: f()
		case 4, 5, 6, 7: f()
	}
		
	switch x := f(); {  // missing switch expression means "true"
	    case x < 0: return -x
	    default: return x
	}
		
	switch {
		case 1 < 2: f()
		case 2 < 3: f()
		case tag == 4: f()
	}

	return 1
}