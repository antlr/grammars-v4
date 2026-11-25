// Tests statement expressions and assignments in control flow.

package sample.statementexpressions

object NamedStatementObject {
}

fun statementExpressions(values: List<Int>, initial: Int) {
    var value = initial
    value = value + 1
    value += 1
    if (value > 0) value = value - 1
    while (value < 10) value += 1
    itemsLoop@ for (item in values) value = item
    do value += 1 while (value < 20)
    val selected = when (val subject = value) {
        0 -> 0
        else -> subject
    }
}
