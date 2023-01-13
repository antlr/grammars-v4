import kotlin.test.*
fun baz(name: String): Unit {

}
fun box(): String {
assertEquals(listOf("parameter #0 name of ${::baz}"), (when {
(::baz) !in (try {
null
}
finally {

}) -> (::baz)
else -> (::baz)
}).parameters.map(Any::toString))
return "OK"
}