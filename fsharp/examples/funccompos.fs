let add x : int = x + 10
let subtract x = x - 5
let multiply x = x * 2
 
let n = 10
let result = n |> add |> subtract |> multiply
 
printfn $"result = {result}"        // result = 30