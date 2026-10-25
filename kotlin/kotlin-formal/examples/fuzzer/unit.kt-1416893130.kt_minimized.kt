import kotlin.reflect.jvm.*
import kotlin.test.assertEquals
var bar: Unit = Unit
fun box(): String {
assertEquals(Unit::class.java, (when {
(::bar) in null -> (::bar)
else -> (::bar)
}).setter.parameters.single().type.javaType)
return "OK"
}