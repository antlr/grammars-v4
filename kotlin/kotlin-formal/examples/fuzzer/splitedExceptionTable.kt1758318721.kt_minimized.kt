inline fun test(s: (() -> Int)): Unit {
var i = 0
try {
i = (s)!!()
}
finally {

}
}
fun box(): String {
var p: Int = 1
test({try {
return "OK"
}catch(e: Exception) {
p
}
finally {

}})
return "fail"
}