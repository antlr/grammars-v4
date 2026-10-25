var bar: Unit = Unit
tailrec fun box(): String {
assertEquals(Unit::class.java, (if ((::bar) == (::bar)) {
(::bar)
} else {

}).setter.parameters.single().type.javaType)
}