inline fun <R> inlineRun(block: (() -> R)) = block()
class Outer(val outerProp: String) {
fun foo(arg: String): (String)? {
class Local {
val work1 = run({})
val work2 = inlineRun({})
val obj = object: Any(){
override inline fun toString() = outerProp + arg
}
override fun toString() = "${work1}#${work2}#${obj.toString()}"
}
return Local().toString()
}
}