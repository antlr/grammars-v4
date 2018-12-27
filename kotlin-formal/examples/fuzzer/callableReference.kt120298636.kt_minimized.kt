fun box(): String {
val f = (((if (true) {
(::foo)
} else {
(::foo)
}))!!)()
}