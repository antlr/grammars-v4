-- Implicit Concurrency Example: dice throw (reference, pp.17-18)
def toss() = Random(6) + 1 -- return random n, 1 <= n <= 6

def exp(0) = 0
def exp(n) = exp(n - 1)
           + (if toss() + toss() = 7 then 1 else 0)

exp(3)
