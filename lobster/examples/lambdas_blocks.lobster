// fn lambdas passed as values, and a function taking a trailing block
// (function-value) argument, called without an explicit "fn" keyword.

def apply_twice(f, x):
    return f(f(x))

def repeat(n:int, f):
    for(n) i: f(i)

def main():
    let double = fn(x): x * 2
    print(apply_twice(double, 5))
    repeat(3) i: print "iteration {i}"

main()
