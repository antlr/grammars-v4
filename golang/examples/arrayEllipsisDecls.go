package samples

import (
	"fmt"
)

type Custom struct {
	string
}

func ArrayEllipsisDecls() {
	stooges := [...]Custom{{"Moe"}, {"Larry"}, {"Curly"}} // len(stooges) == 3
	fmt.Println("Stooges: ", stooges)
}