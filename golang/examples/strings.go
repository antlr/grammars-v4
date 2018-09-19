// Verifies raw and interpreted string parsing (see issue #1131).
package main

import (
	"fmt"
)

type Vertex struct {
	X float64 `"X" Description` /* X Comment */
	Y float64 "`Y` Description" // Y Comment
}

func main() {
	fmt.Println("this \"that")
	fmt.Println(`Hello`)
}

type Something struct {
	Msg    string    `World`
}
