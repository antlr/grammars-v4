package samples

import "fmt"

// var entries map[string]int

func constMultiRet() (int, int) {
	return 1, 2
}

func ConstDecls() {
	const (
		c0 = iota  // c0 == 0
		c1 = iota  // c1 == 1
		c2 = iota  // c2 == 2
	)
	const (
		Sunday = iota
		Monday
		Tuesday
		Wednesday
		Thursday
		Friday
		Partyday
		numberOfDays  // this constant is not exported
	) // TODO: Added support for iota token
	const Pi float64 = 3.14159265358979323846
	const zero = 0.0         // untyped floating-point constant
	const (
		size int64 = 1024
		eof        = -1  // untyped integer constant
	)
	const a, b, c = 3, 4, "foo"  // a = 3, b = 4, c = "foo", untyped integer and string constants
	const u, v float32 = 0, 3    // u = 0.0, v = 3.0
	fmt.Println(c0, c1, c2, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Partyday, numberOfDays, Pi, zero, size, eof, a, b, c, u, v)
}
