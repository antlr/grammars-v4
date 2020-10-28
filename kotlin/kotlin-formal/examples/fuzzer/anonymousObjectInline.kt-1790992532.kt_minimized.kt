class A {
fun foo(): Unit {
inlineFun({})
}
inline fun inlineFun(lambda: (() -> Unit)): Unit {
val s = object{
fun run(): Unit {
(lambda)!!()
}
}
}
}