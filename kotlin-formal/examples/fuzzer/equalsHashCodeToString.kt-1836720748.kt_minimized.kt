data class D(val s: String)
fun Any.box(): String {
assert(D::hashCode.call((try {

}
finally {

})("foo")) == (if ((D::hashCode) !in (D::hashCode)) {
(D::hashCode)
} else {

}).(((call(D("foo"))))!! ?: (call(D("foo")))))
}