package samples

import "fmt"

var entries map[string]int

func multiRet() (int, int) {
	return 1, 2
}

func VarDecls() {
	var a int        // +
	var b, c float64 // + strange extra levels
	var d = 1        // + doesn't show zero value
	var e, f float32 = -1, -2  // +
	var (
		g       int
		h, i, j = 2.0, 3.0, "bar"
	) // + need to precise general text span
	// var _, k = entries["1"] // map lookup;
	var l, m = multiRet() // + TODO: Figure out with duplication
	fmt.Println(a, b, c, d, e, f, g, h, i, j, /*k,*/ l, m)
}
