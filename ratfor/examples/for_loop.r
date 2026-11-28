# sum the squares of 1..10 using a C-like for loop
integer i, total
total = 0
for (i = 1; i <= 10; i = i + 1) {
    total = total + i ** 2
}
print *, total
