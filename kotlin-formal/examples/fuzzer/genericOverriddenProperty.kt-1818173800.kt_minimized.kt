sealed interface H<T> {
val parent: (T)?
}
interface A: H<A>



fun box(): String {
assertEquals("T?", (if ((H<A>::parent) >= (H<A>::parent)) {
(H<A>::parent)
} else {

}).returnType.toString())
}