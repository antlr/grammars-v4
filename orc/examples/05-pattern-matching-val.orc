-- Pattern Matching in val (reference, p.8)
val (x, y) = (2 + 3, 2 * 3)
val (a, b) = ("one", "two")
val ((c, d), e) = ((1, true), [2, false])
val (f, _, _) = (1, (2, 2), [3, 3, 3])
val [[_, g], [_, h]] = [[1, 3], [2, 4]]
(x, y, a, b, c, d, e, f, g, h)
