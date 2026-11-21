-- Closure, repeat forever (reference, p.12)
def repeat(f) = f() >> repeat(f)
def pr() = Println(3)

repeat(pr)
