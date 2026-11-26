// struct declaration (the type name is lower-case; its auto-generated
// constructor function is the capitalized "Point"), and field access.

struct point {
    x : int;
    y : int
}

fun dist(p : point) : int {
    p.x + p.y
}

fun main() {
    val p = Point(3, 4);
    println(dist(p).show)
}
