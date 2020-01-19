fun B.bar(): String {
val x = foo(A("O", "K", 123))((if (true) {
({x, y, z -> })
} else {
({x, y, z -> })
}))
}