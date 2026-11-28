# define and call a subroutine
integer x, y
x = 3
y = 4
call hypotsq(x, y)
stop

subroutine hypotsq(a, b)
integer a, b, s
s = a * a + b * b
print *, s
return
end
