class B: A() {
override fun foo(x: Int, y: Int): Unit {

}
}
fun box(): String {
assertEquals(listOf(false, false, true), (if ((B::foo) != false) {
(B::foo)
} else {

}).parameters.map({}))
}