package main

import "fmt"

type Person struct {
    name string
    age int32
}

func (p Person) IsAdult() bool {
    return p.age >= 18
}

type Employee struct {
    position string
}

func (e Employee) IsManager() bool {
    return e.position == "manager"
}

// In the following structure declaration, Employee is a
// promoted structure, not the "result" type of Person
type Record struct {
    Person
    Employee
}

func main() {
    person := Person{name: "Michal", age: 29}
    fmt.Println(person)  // {Michal 29}
    record := Record{}
    record.name = "Michal"
    record.age = 29
    record.position = "software engineer"

    fmt.Println(record) // {{Michal 29} {software engineer}}
    fmt.Println(record.name) // Michal
    fmt.Println(record.age) // 29
    fmt.Println(record.position) // software engineer
    fmt.Println(record.IsAdult()) // true
    fmt.Println(record.IsManager()) // false
}