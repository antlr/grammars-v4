// A type with constructors that carry fields, and a match expression
// that destructures them.

type shape {
    Circle(radius : float64);
    Square(side : float64)
}

fun area(s : shape) : float64 {
    match s {
        Circle(r) -> 3.14159 * r * r;
        Square(side) -> side * side
    }
}

fun main() {
    println(area(Circle(2.0)).show)
}
