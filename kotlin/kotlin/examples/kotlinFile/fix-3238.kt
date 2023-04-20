fun <T : Any> T?.f(t: T): T = t

interface I1<T : Any> {

    interface I2<S : Any> {
        fun g()

        interface I3<S : Any>{
            fun g()
        }
    }

    fun f()
}

class C {

    fun <T: Any> I1<T>?.f(value: Any?): Any? {
        return null
    }

    fun <S : Any> I1.I2.I3<S>.g(value: Any?): Any? {
        return null
    }
}
