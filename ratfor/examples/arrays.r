# arrays declared with DIMENSION, summed into a total
integer i, total
dimension values(10)
integer values
do i = 1, 10, 1
    values(i) = i * i
total = 0
for (i = 1; i <= 10; i = i + 1) {
    total = total + values(i)
}
print *, total
