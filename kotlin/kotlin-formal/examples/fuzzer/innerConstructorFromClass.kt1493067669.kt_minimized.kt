class A {
inner class Inner {

}
fun result() = ((if ((A::Inner) >= null) {
(A::Inner)
} else {

}))(this).o + (A::Inner)(this).k
}