infix fun box(): String {
assertEquals(KVariance.INVARIANT, ((if (true) {
(::listOfStrings)
} else {
(::listOfStrings)
}))!!.returnType.arguments.first().variance)
}