package samples

import "fmt"
import . "time"

func AnonymousMethods() {
	lambd := func(s string) { Sleep(10); fmt.Println(s) }
	lambd("From lambda!")
	func() { fmt.Println("Create and invoke!")}()
}
