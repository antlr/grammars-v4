package samples

import "fmt"

func goRoutineA(a <-chan int) {
	val := <-a
	fmt.Println("goRoutineA received the data", val)
}

func BufChan() {
	c := make(chan int, 2) // a buffered channel
	c <- 3
	c <- 5
	close(c)
	fmt.Println(len(c), cap(c)) // 2 2
	x, ok := <-c
	fmt.Println(x, ok)          // 3 true
	fmt.Println(len(c), cap(c)) // 1 2
	x, ok = <-c
	fmt.Println(x, ok)          // 5 true
	fmt.Println(len(c), cap(c)) // 0 2
	x, ok = <-c
	fmt.Println(x, ok) // 0 false
	x, ok = <-c
	fmt.Println(x, ok)          // 0 false
	fmt.Println(len(c), cap(c)) // 0 2
	// close(c) // panic!
	// The send will also panic if the above
	// close call is removed.
	// c <- 7

	ch := make(chan int)
	go goRoutineA(ch)
}
