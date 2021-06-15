var result = "fail"
object TimeUtil {
fun waitForAssert(z: String): Unit {
waitForEx(action={result})
}
inline fun waitForEx(retryWait: Int = 200, action: (() -> String)): Unit {
(action)!!()
}
}