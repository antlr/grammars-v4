interface A<T> {
fun foo(t: T): String
}
sealed interface B {
inline tailrec suspend infix operator fun foo(t: Int) = "B"
}
class Z2: B by Z(), A<Int>