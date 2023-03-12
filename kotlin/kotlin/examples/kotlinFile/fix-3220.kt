interface IG<T, S, U> {
    fun f(t: T)
}

fun <D> IG<D, *, *>.f(d: D): Any? {
    return null
}
