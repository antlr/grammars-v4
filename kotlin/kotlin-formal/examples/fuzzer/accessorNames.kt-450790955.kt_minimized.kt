fun box(): String {
assertEquals("<get-foo>", ((if (true) {
(::foo)
} else {
(::foo)
})!!).getter.name)
}