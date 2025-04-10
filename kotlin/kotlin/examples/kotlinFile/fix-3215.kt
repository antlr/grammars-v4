interface I1<T : Any> {

    interface I2<S : Any> {
        fun g()
    }

    fun f()
}

class C {

    fun <T: Any> I1<T>.f(value: Any?): Any? {
        return null
    }

    fun <S : Any> I1.I2<S>.g(value: Any?): Any? {
        return null
    }
}
