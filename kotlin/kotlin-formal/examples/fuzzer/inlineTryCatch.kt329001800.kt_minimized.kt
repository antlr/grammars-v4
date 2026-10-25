inline fun <T> tryOrElse(f1: (() -> T), f2: (() -> T)): T {
try {
return (f1)!!()
}catch(e: Exception) {
return f2()
}
}
fun testIt() = "abc" + tryOrElse({}, {}) + "ghi"