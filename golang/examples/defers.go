package samples

import (
	"fmt"
	"crypto/rsa"
	"crypto/rand"
)

func DeferCallee() {
	fmt.Println("Called from the first defer!")
}

func DeferCallee2() {
	fmt.Println("Called from the second defer!")
}

func Defers() {
	pvk, err := rsa.GenerateKey(rand.Reader, 2048)
	defer DeferCallee()
	// switch the equal sign for the different number of executed defer invocations
	if extra := -1; err != nil {
		fmt.Println("Something went wrong", extra)
	} else {
		return
	}

	defer DeferCallee2()
	fmt.Printf("End\n", pvk.Size())
}
