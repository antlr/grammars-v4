fun box(): String {
assertEquals(listOf(false, false, true), ((B::foo)!!)!!.parameters.map({}))
}