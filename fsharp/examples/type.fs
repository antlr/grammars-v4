type operation = float ->float-> float
 
let sum: operation = fun x y -> x + y
let action x y (func: operation) = func x y
  
let result1 = action 10.0 6.0 sum      // 16
printfn $"result1 = {result1}"    // result1 = 16