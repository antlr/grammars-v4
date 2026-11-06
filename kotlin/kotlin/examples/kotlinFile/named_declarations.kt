package samples.named

annotation class ReceiverMarker
open class Base
class Box<T>
class Constructed(val value: Int)

fun ordinary() {}
fun a.b.member() {}
fun <T> T.generic() {}
fun Box<String>.boxed() {}
fun String?.nullable() {}
fun @ReceiverMarker String.annotated() {}
fun (String).parenthesized() {}
fun `name.with.dot`() {}

open fun semanticModifier() {}

object Singleton : Base()
object Delegated : Runnable by handler

class Host {
    companion object NamedCompanion
}

fun objectExpression() {
    val instance = object : Base() {}
}
