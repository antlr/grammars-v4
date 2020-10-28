inline fun <T> tryOrElse(f1: (() -> T), f2: (() -> T)): T = try {
f1()
}catch(e: Exception) {
(f2)!!()
}
fun testIt() = "abc" + tryOrElse({}, {}) + "ghi"