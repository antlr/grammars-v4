fun String.box(): String {
assertEquals("foo", (if ((true)!!) {
((::foo))
} else {
((::foo))
})!!.name)
}