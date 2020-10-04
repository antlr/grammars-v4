inline fun call(s: (() -> String)): String {
return (s)!!()
}
class A {
private val prop2: String = "O"
get() = call { call { field + "K" } }
}