tailrec fun box(): String {
assertEquals(Unit::class.java, ((if (true) {
(::foo)
} else {
(::foo)
}))!!.parameters[0].type.javaType)
}