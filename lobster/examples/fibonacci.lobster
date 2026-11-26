// Recursive Fibonacci, using a typed function with an explicit return type.

def fib(n:int) -> int:
    if n < 2: return n
    else: return fib(n - 1) + fib(n - 2)

def main():
    for(10) i:
        print "fib({i}) = {fib(i)}"

main()
