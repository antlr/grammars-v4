package samples

import "fmt"

type Vertex2 struct {
	Lat, Long float64
}

var s map[int]string
var m map[string]Vertex2

func Maps() {
	m = make(map[string]Vertex2)
	m["Bell Labs"] = Vertex2{
		40.68433, -74.39967,
	}
	s[1] ="test"
	fmt.Println(m["Bell Labs"])
}