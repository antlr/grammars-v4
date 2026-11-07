package samples.anonymousfunctions

fun typed(value: Int) = fun(item: Int): Int { return item + value }

fun inferred(value: Int) = fun(item) = item + value

fun emptyParameters() = fun() = 1

fun extension() = fun String.(suffix: String): String = this + suffix

fun trailingComma() = fun(first: Int, second,): Int = first + second

fun nested() = consume(fun(first: Int) = fun(second) = first + second)

fun invoked() = (fun(value: Int) = value * 2)(21)

fun inTemplate(value: Int) = "${fun(item: Int) = item + value}"