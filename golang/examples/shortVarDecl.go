package samples

import "fmt"
import "os"

func ShortVarDecls() {
	a, b := 0, 10
	c := func() int { return 7 }
	d := make(chan int)
	e, f, _ := os.Pipe()  // os.Pipe() returns a connected pair of Files and an error, if any
	// _, y, _ := coord(p)   // coord() returns three values; only interested in y coordinate
	fmt.Println(a, b, c, d, e, f)
}
