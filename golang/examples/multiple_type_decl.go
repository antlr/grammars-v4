package main

import (
	"fmt"
	"time"
)

type (
	Abser interface {
		Abs() float64
	}
	
	MyError struct {
		When time.Time
		What string
	}

	MyCustomError struct {
		Message string
		Abser
		MyError
	}
)

// The following takes precedence over instance call to Abs()
func (myErr *MyCustomError) Abs() float64 {
	return 0.0
}

func main() {
	a:= MyCustomError{"New One", nil, MyError{time.Now(), "Hello"}}
	a.Abs()
	a.Message = "New"
	fmt.Println("MyCustomError method = %v", a.Abs())
}