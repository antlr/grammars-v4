annotation class TypeMarker

class FunctionSupertype : (String) -> Unit
class DelegatedFunctionSupertype : (String) -> Unit by handler
object FunctionObject : () -> Unit
object DelegatedFunctionObject : () -> Unit by callback

open class BaseType
interface Named
interface DelegatedType
class ExistingSupertypes : BaseType(), Named, DelegatedType by delegate

fun catchTypes() {
    try {
    } catch (function: (String) -> Unit) {
    } catch (nullable: String?) {
    } catch (annotated: @TypeMarker String) {
    } catch (parenthesized: (String)) {
    }
}

fun castTypes(value: Any) {
    val function = value as (String) -> Unit
    val nullable = value as String?
    val annotated = value as @TypeMarker String
}

var blockSetter: Int = 0
    set(value)
        : Unit
    {
        field = value
    }

var expressionSetter: Int = 0
    set(value: Int,): kotlin.Unit = value

var untypedSetter: Int = 0
    set(value) {
        field = value
    }
