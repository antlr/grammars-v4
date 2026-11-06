typealias NonNull<T> = T & Any

class Holder<T>(val value: T & Any)

fun <T> identity(value: T & Any): T & Any = value

fun <T> annotated(value: T & @Marker Any): T & Any = value

fun <T> parenthesized(value: ((T)) & Any): T & Any = value

fun <T> projected(values: List<out T & Any>) {}

fun <T> transform(block: (T & Any) -> T & Any) {}

fun <T> cast(value: Any): T & Any = value as T & Any

fun <T> test(value: Any) = value is T & Any

fun <T> (T & Any).normalize(): String = ""