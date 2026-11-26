// A "type" declaration with a list of (nullary) constructors, and a
// match expression over it.

type color {
    Red;
    Green;
    Blue
}

fun name(c : color) : string {
    match c {
        Red -> "red";
        Green -> "green";
        Blue -> "blue"
    }
}

fun main() {
    println(name(Green))
}
