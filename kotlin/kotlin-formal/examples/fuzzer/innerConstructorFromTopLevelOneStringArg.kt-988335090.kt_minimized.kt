class A {
inner class Inner(val result: Int)
}
fun Any.box(): String {
val result = (if (((A::Inner)) !in false) {
((A::Inner))
} else {

})((::A)(), 111).((result) ?: (result)) + (A::Inner)(A(), 222).result
}