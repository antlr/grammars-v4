# emulate multiplication by additions
x = 0
y = 4

result = 0
i = 0
while i < x:
    result = result + y
    i = i + 1

print("x:")
print(x)
print("y:")
print(y)
print("The product of the two numbers is:")
print(result)
