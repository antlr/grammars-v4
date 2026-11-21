-- Parallel or (reference, p.26)
val z =
    val x = f
    val y = g

    Ift(x) >> true | Ift(y) >> true | (x || y)
z
