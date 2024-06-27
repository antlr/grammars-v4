open System.IO
 
task {
    let! text = File.ReadAllTextAsync("hello.txt")
    printfn "%s" text
}

let getNumber n = async {
    Async.Sleep(500) |> Async.RunSynchronously
    return n * n
} 
let printNumber n =
    async {
        printfn "Получение квадрата числа %d" n
        let! result = getNumber n
        printfn "Квадрат числа %d равен %d" n result
    }
     
 
printNumber 1 |> Async.RunSynchronously
printNumber 2 |> Async.RunSynchronously
printNumber 3 |> Async.RunSynchronously