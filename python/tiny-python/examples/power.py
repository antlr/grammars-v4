# emulate power by additions
base = 2
exponent = 10

result = 1
e = 0
while e < exponent:
    partial = result
    b = 1
    while b < base:
        result = result + partial
        b = b + 1
    e = e + 1

print("Base:")
print(base)
print("Exponent:")
print(exponent)
print("The power of the two numbers is:")
print(result)
