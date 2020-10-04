package samples

import ("fmt"
	"reflect"
)

func ParamTest(a, b int32, c int16) {
	fmt.Printf("Type of a is %s\n", reflect.TypeOf(a))
	fmt.Printf("Type of a is %s\n", reflect.TypeOf(b))
	fmt.Printf("Type of a is %s\n", reflect.TypeOf(c))
}