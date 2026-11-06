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
}
