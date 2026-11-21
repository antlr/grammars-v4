-- Simple Swap using Refs (reference, p.29)
def swap(a, b) = (a?, b?) > (x, y) > (a := y, b := x) >> signal

swap(Ref(1), Ref(2))
