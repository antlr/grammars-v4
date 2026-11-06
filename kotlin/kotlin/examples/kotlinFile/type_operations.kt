@Target(AnnotationTarget.TYPE)
annotation class TypeMarker

fun typeOperations(value: Any, items: List<Any>, excluded: List<Any>) {
    val nullable = value as String?
    val qualified = value as kotlin.String
    val generic = value as List<String>
    val parenthesized = value as (String)
    val function = value as (String) -> Int
    val annotated = value as @TypeMarker String
    val suspendFunction = value as suspend () -> String
    val mixed = value in items is String !in excluded !is Number
    val casts = value as List<String> as? List<Any>
    val continued = value as
        String
}
