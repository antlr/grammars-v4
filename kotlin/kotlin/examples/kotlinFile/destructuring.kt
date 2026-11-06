[val topLevelName, val topLevelAge] = Pair("Ada", 36)

fun destructuring(values: List<Pair<String, Int>>) {
    val [positionalName, positionalAge] = values[0]
    val [positionalTrailingComma,] = values[0]
    val (namedAge = age, namedName = name) = values[0]
    val (namedTrailingComma = name,) = values[0]

    [val fullName: String, var fullAge: Int,] = values[0]
    (
        val fullNamedName = name,
        var fullNamedAge: Int = age,
    ) = values[0]

    for ([loopName, loopAge] in values) {
    }
    for ((name = namedLoopName, age = namedLoopAge) in values) {
    }
    for ([val fullLoopName, var fullLoopAge] in values) {
    }
    for ((val fullNamedLoopName = name, var fullNamedLoopAge = age) in values) {
    }

    values.forEach { [lambdaName, lambdaAge] -> }
    values.forEach { (name = namedLambdaName, age = namedLambdaAge) -> }
    values.forEach { [val fullLambdaName, var fullLambdaAge]: Pair<String, Int> -> }
    values.forEach { (val fullNamedLambdaName = name, var fullNamedLambdaAge = age) -> }
}