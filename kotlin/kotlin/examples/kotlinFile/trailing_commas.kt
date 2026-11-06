typealias EmptyFunction = () -> Unit
typealias NamedFunction = (value: String,) -> Unit
typealias UnnamedFunction = (String, Int,) -> Unit

fun lambdaExamples() {
    val empty = { -> Unit }
    val single = { value, -> value }
    val multiple = { first: Int, second, -> first + second }
    val destructured = { (first, second), -> first + second }
}

fun indexing(values: List<Int>) {
    values[0,]
    values[0, 1,]
}

fun conditions(value: Any) {
    when (value) {
        1, 2, -> Unit
        is String, !is Int, -> Unit
    }
}

fun catches() {
    try {
        Unit
    } catch (problem: Exception,) {
        Unit
    }
}

var count: Int = 0
    set(value: Int,) {
        field = value
    }
