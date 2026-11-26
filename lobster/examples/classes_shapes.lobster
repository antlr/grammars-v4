// An abstract base class with two subclasses (reference types,
// single inheritance via the ": Base" clause).

abstract class Shape:
    def area() -> float: return 0.0

class Circle : Shape
    r:float
    def area() -> float: return 3.14159 * r * r

class Rect : Shape
    w:float
    h:float
    def area() -> float: return w * h

def main():
    let c = Circle { r: 2.0 }
    let r = Rect { w: 3.0, h: 4.0 }
    print "circle area = {c.area()}"
    print "rect area = {r.area()}"

main()
