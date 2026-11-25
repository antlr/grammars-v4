// Tests escaped and raw strings, characters, and identifiers.

package sample.stringliterals

fun stringLiterals(name: String) {
    val ordinaryEscapes = "tab\t backspace\b newline\n return\r quote\' double\" backslash\\ dollar\$ unicode\u0041"
    val rawBackslashes = """raw slash: \; raw escape-looking text: \n; reference after slash: \$name; expression after slash: \${name}; trailing slash before close: \"""
    val characters = listOf('a', '\n', '\u0041', '\\', '\'', '\"', '\$')
    val escapedIdentifier = `single line identifier`
}
