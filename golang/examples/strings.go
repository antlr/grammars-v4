// Verifies raw and interpreted string parsing (see issue #1131).
package main

import (
	"fmt"
)

func main() {
	fmt.Println("this \"that")
	fmt.Println(`Hello`)
}

type Something struct {
	Msg    string    `World`
}
