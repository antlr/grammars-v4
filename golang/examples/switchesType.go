package samples

import "fmt"

type O struct {

}

func (O) typee() {} 

func TypeSwitch(i interface{}) {
	switch v := i.(type) {
	case int:
		fmt.Printf("Twice %v is %v\n", v, v*2)
	case string:
		fmt.Printf("%q is %v bytes long\n", v, len(v))
	case byte, uintptr:
		fmt.Printf("Type i %T", v)
	default:
		fmt.Printf("I don't know about type %T!\n", v)
	}
}
