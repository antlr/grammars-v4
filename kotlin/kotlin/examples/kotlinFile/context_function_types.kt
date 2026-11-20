// Tests context function types in declarations and expressions.

package sample.contextfunctiontypes

typealias Basic = context(Logger) () -> Unit
typealias Multiple = context(Logger, @TypeMarker Service, List<String?>) suspend () -> String
typealias Multiline = context(
    Logger,
    Service,
)
suspend
() -> String
typealias Receiver = context(Logger) String.(Int) -> Unit
typealias NamedParameters = context(Logger) (input: Int, String) -> Unit
typealias Nested = (context(Logger) () -> Unit)?
typealias Generic = Handler<context(Logger) () -> Unit>

fun accepts(block: context(Logger) (Int) -> String, logger: Logger) {}

class ContextFunctionSupertype : context(Logger) () -> Unit
class ContextFunctionDelegation : context(Logger) () -> Unit by handler

fun cast(value: Any) {
    val result = value as context(Logger) () -> Unit
}

fun template() = "${consume< context(Logger) () -> Unit > { it }}"

context(logger: Logger)
fun declarationContext(block: context(Logger) () -> Unit) {}
