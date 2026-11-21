-- Closure: Functions as values (reference, p.12)
val minmax = (min, max)

def apply2((f, g), (x, y)) = (f(x, y), g(x, y))

def pmap(f, []) = []
def pmap(f, x : xs) = f(x) : pmap(f, xs)

apply2(minmax, (2, 1))
| pmap(lambda(i) = i * i, [2, 3, 5])
