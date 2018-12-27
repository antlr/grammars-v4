inline fun test(f: ((String) -> Unit)): Unit {
testNested((if (true) {
({it -> })
} else {
({it -> })
}))
}