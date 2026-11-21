-- Timeout (reference, p.20)
val (z, b) = (M(), true) | (Rwait(t), false)
if b then f(z) else g
