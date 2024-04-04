let operation n = 
    if n = 1 then fun x y -> x + y
    elif n = 2 then fun x y -> x - y
    elif n = 3 then fun x y -> x * y
    else fun x y -> 0
 
let selection = operation 1
 
let result1 = selection 10 7    // 17
printfn $"result1 = {result1}"  // result1 = 17
 
let result2 = operation 2 10 7  // 3
printfn $"result2 = {result2}"  // result2 = 3