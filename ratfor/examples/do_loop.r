# Fortran-style DO loop, carried over from Fortran into Ratfor
integer i, total
total = 0
do i = 1, 10, 1
    total = total + i
print *, total
