package operandname

import (
	"github.com/cornelk/hashmap"
)

func foo()  {
	// any package to get `identifier dot identifier` in operandName rule
	x := hashmap.New[int, string]() 	
}
