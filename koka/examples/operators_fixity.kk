// A fixity declaration, and user-defined operators (here, the built-in
// "++" string append) used in ordinary arithmetic/string expressions.

infixl 6 (+++);

fun main() {
    val a = 2 + 3 * 4;
    val b = (2 + 3) * 4;
    println(a.show ++ " " ++ b.show)
}
