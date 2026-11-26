// Koka's signature feature: an effect declaration, a function whose type
// mentions the effect it performs ("<emit> unit"), and a "handle"
// expression that supplies a handler for it.

effect emit {
    fun emit(msg : string) : unit
}

fun greet() : <emit> unit {
    emit("hello");
    emit("world")
}

fun main() {
    handle(greet()) {
        fun emit(msg : string) -> println(msg)
    }
}
