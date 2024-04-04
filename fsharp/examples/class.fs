let mutable count = 0
type Person() = 
    do
        printfn "Создание объекта класса Person"
        count <- count + 1
        printfn $"Person {count}"
 
let tom = Person()
let bob = Person()