package samples

import "fmt"

type T1 int
type T22 T1
type T3 T22
type T4 T3

// An empty struct.
type EmptyStruct struct {}

// A struct with 6 fields.
type regularStruct struct {
	z, zz int
	u float32
	_ float32  // padding
	A *[]int
	F func()
}

// A struct with four embedded fields of types T1, *T2, P.T3 and *P.T4
type PromotedStruct struct {
	T1        // field name is T1
	*T22     // field name is T22
	/*regularStruct.z      // field name is T3
	*regularStruct.zz     // field name is T4*/
	x, y int  // field names are x and y
}

type StructWithTag struct {
	x, y float64 ""  // an empty tag string is like an absent tag
	name string  "any string is permitted as a tag"
	_    [4]byte "ceci n'est pas un champ de structure"
}

// A struct corresponding to a TimeStamp protocol buffer.
// The tag strings define the protocol buffer field numbers;
// they follow the convention outlined by the reflect package.
type StructWithNumberFields struct {
	microsec  uint64 `protobuf:"1"`
	serverIP6 uint64 `protobuf:"2"`
}

type person struct {
	name string
	age  int
	personfunc func()
}

func (*person) outside() {
	fmt.Println("Declared outside and invoked!!!")
}

func Structs() {
	fPtr := func() { fmt.Println("LAUNCH FROM  PERSON STRUCT INSTANCE") }

	fmt.Println(person{"Bob", 20, fPtr})

	fmt.Println(person{name: "Alice", age: 30})

	fmt.Println(person{name: "Fred"})

	fmt.Println(&person{name: "Ann", age: 40})

	s := person{name: "Sean", age: 50, personfunc: fPtr}
	fmt.Println(s.name)

	s.personfunc()
	s.outside();

	sp := &s
	fmt.Println(sp.age)

	sp.age = 51
	fmt.Println(sp.age)

	var zeroPerson person
	fmt.Println(zeroPerson)

	newPerson := new(person)
	fmt.Println(newPerson)
}
