class A {
inline fun inlineFun(s: (() -> Unit)): Unit {
(s)!!()
}
fun foo(): Unit {
inlineFun({})
}
}