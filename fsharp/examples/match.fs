let number = 1
 
let result = match number with
             | 1 -> "Number is one"
             | 2 -> "Number is two"
             | 3 -> "Number is three"
             | _ -> "Undefined number" 
 
printfn "%s" result     // Number is one