package main

import "fmt"

type Person struct {
    work func()
    name string
    age int32
}

func main() {
    person := Person{work: nil, name: "Michał", age: 29}
    fmt.Println(person)  // {<nil> Michał 29}
}