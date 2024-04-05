let mutable m = 1
let mutable n = 1
while m < 10 do
    while n < 10 do
        printf $"{m * n}\t"
        n <- n + 1
    printfn ""
    m <- m + 1
    n <- 1