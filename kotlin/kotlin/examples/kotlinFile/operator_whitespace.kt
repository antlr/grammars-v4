fun operatorWhitespace() {
    var value = 0
    value =
        1
    value +=
        2
    value -=
        1
    value *=
        2
    value /=
        2
    value %=
        3

    val positive = +
        1
    val negative = -
        1
    val incremented = ++
        value
    val decremented = --
        value
    val negated = !
        false
    val doubleNegated = !
        !true
    val bracketedPrefix = [+
        1
    ]
    val parenthesizedNegation = (!
        false)

    val dotted = value.
        toString()
    val safeDotted = value?.
        toString()
    val indexed = listOf(value)[
        0
    ]
    val lineTemplate = "${value?.
        toString()}"
    val rawTemplate = """${value?.
        toString()}"""
    val templateNegation = "${!
        false}"
    val nullable: String? = null
}
