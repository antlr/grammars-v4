fun g(): Unit {

}
fun box(): String {
assertEquals(::g, ((if ((::g) !in super) {
(::g)
} else {

})!!).javaMethod!!.kotlinFunction)
}