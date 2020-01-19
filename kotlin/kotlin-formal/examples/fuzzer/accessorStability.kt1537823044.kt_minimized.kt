fun call() = inlineFun2({})
internal inline fun inlineFun2(p: (() -> Unit)): String {
((p)!!)!!()
return inlineFun({test()})
}
private fun test() = "OK"
inline internal fun inlineFun(p: (() -> String)): String {
return p()
}