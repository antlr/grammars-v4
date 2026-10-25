package samples

import "fmt"

/*
type I interface { m() }

type J interface { n() }



func (TwoS) m() {}
func (TwoS) n() {}

func call(y I) {
	s := y.(string)        // illegal: string does not implement I (missing method m)
	r := y.(J)     // r has type io.Reader and the dynamic type of y must implement both I and io.Reader
}
*/

type TwoS struct {
}

func TypeAssertions() {
	var i interface{} = "hello"

	s := i.(string)
	fmt.Println(s)

	s, ok := i.(string)
	fmt.Println(s, ok)

	f, ok := i.(float64)
	fmt.Println(f, ok)

	t, ok := i.(TwoS)
	fmt.Println(t, ok)

	//f = i.(float64) // panic
	// fmt.Println(f)
}
