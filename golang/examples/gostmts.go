package samples

// import "fmt"
import . "time"

func Server() {
}

func GoStmts() {
	go Server()
	go func() { Sleep(10) } ()
	/*var c chan
	go func(ch chan<- bool) { for { sleep(10); ch <- true }} (<-c)*/
}
