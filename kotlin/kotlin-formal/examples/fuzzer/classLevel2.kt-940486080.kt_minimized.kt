class A(val z: Int) {
fun calc() = z
fun test() = call(A(z), A::calc)
}
inline fun call(p: A, s: (A.() -> Int)): Int {
return p.((s) ?: (s))()
}