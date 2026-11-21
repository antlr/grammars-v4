-- Library function Table, and Random-based definitions (reference, pp.23, 36)
def rbool() = (Random(2) = 0)
def frandom() = Random(1001) / 1000.0
def biasedBool(p) = (Random(1000) <: p * 1000)

val g = Table(5, lambda(_) = Channel())
val h = Table(5, lambda(i) = 2 * i)

(rbool(), frandom(), biasedBool(5), g, h)
