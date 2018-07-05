package main

import "fmt"

type T1 struct {
    name string
}

func (t T1) M() {}
func (t T1) N() {}
func (t T1) String() string { return "" }
func (t T1) Error() string { return "" }

type T2 struct {
    name string
}

func (t T2) M() {}
func (t T2) N() {}
func (t T2) String() string { return "" }
func (t T2) Error() string { return "" }

type I interface {
    M()
}

// In the following interface declaration, error is an
// inherited interface, not the "result" type of N()
type V interface {
    I
    fmt.Stringer
    N()
    error
}

func main() {
    m := make(map[I]int)
    var i1 I = T1{"foo"}
    var i2 I = T2{"bar"}
    m[i1] = 1
    m[i2] = 2
    fmt.Println(m)

    n := make(map[V]int)
    var v1 V = T1{"foo"}
    var v2 V = T2{"bar"}
    v1.N()
    v2.M()
    v1.String()
    v2.Error()
    n[v1] = 3
    n[v2] = 4
    fmt.Println(n)
}