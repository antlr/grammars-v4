// глобальная область видимости

let a = 5
let helloGlobal() = printfn "Global scope"
 
let outer() =       // область видимости функции outer
 
    let inner() =              // область видимости функции inner
        helloGlobal()         // обращение к фунции helloGlobal из глобального контекста
        printfn $"Inner scope. a: {a}"      // обращение к значению a из глобального контекста
 
    inner()
    printfn "Outer scope"
 
outer()