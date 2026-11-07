// Tests newline handling in expressions, blocks, and string templates.

package sample.blocknewlines

fun blockNewlines() {
    val parenthesizedExpression = (1
        + 2)
    val bracketedExpression = [1
        + 2]
    val parenthesized = ({
        val first = 1
        first + 1
    })
    val bracketed = [
        {
            val second = 2
            second + 1
        }
    ]
    val nested = ({
        {
            3
            4
        }
        5
    })
    val template = """${({
        val sixth = 6
        sixth + 1
    })}"""
    val lineTemplateBlock = "${{
        val seventh = 7
        val nested = {
            val eighth = 8
            eighth
        }
        seventh + nested()
    }}suffix"
    val rawTemplateBlock = """${{
        val ninth = 9
        val nested = {
            val tenth = 10
            tenth
        }
        ninth + nested()
    }}suffix"""
}
