# a recursive factorial function
integer n, result
n = 5
result = fact(n)
print *, result
stop

integer function fact(n)
integer n
if (n <= 1) {
    fact = 1
}
else {
    fact = n * fact(n - 1)
}
return
end
