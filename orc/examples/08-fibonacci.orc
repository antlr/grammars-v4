-- Clausal definition of a function: Fibonacci numbers (reference, p.11)
def H(0) = (1, 1)
def H(n) = H(n - 1) > (x, y) > (y, x + y)

def Fib(n) = H(n) > (x, _) > x

{- Goal expression -}
Fib(5)
