package samples

import (
	"fmt"
	"net/http"
)

type Foo1 func(x int) int

type Foo2 func(x int) int

type Request struct{}

// T is a custom type
type T2 struct {
	http.Request // field name is "Request"
	age          int32
}

func Types() {
	// person := T{name: "Michael", age: 29}
	// fmt.Println(person)
	fmt.Println("123")
}
