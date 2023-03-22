open class CC<T> {
    open fun f() {}
}

fun fo(c: CC<Int>) {
    c.f()
}

class CCC {
    fun dff() {
        fo(object : CC<Int>() {
            override fun f() {}
        }

        )
    }
}
