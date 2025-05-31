x, y = y, x

a = b
b = a

t[1], t[2] = t[2], t[1]

t[1] = t[2]
t[2] = t[1]

foo().a = foo().b
foo().b = foo().a

-- We use a weird hack so this comment might break something, oh no!
a = b
b = a
