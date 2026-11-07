// Tests negated type and membership operators with trivia.

package sample.negatedoperators

fun negatedOperators(value: Any, values: List<Int>, array: Array<Boolean>) {
    val defaultType = value !is/* block */String
    val defaultMembership = 1 !in /* outer /* nested */ comment */ values
    val parenthesizedType = (value !is /* parenthesized */ String)
    val indexedMembership = array[value !in /* bracket */ values]
    val lineCommentType = value !is // line comment
        String
    val crlfMembership = 1 !in /* mixed */
        /* trivia */ values
    val lineTemplate = "${value !is/* line template */String}"
    val rawTemplate = """${1 !in /* raw template */ values}"""
    val restoredMode = "${value !is /* interpolation */ String}" + (1 !in /* after template */ values).toString()
}
