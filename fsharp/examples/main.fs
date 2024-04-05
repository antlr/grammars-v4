let sum x y = printfn $"Сумма {x} и {y} равна {x + y}"
let printMessage _ = printfn "Hello F#"
 
[<EntryPoint>]
let main _ =
    printfn "Функция main"
    sum 1 2
    printMessage()
    0