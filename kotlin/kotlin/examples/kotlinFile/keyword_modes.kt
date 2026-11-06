open class KeywordModeBase

class KeywordModes : KeywordModeBase() {
    fun check(value: Any?, values: List<Any>): String {
        val cast = (value as String)
        val safeCast = (value as? String)
        val typeCheck = (value is String)
        val notTypeCheck = (value !is String)
        val membership = (value in values)
        val notMembership = (value !in values)
        val indexed = values[(value as Int)]
        val receiver = this.toString()
        val parent = super.toString()
        val lineTemplate = "${value as String} ${value is String} ${value in values}"
        val multilineTemplate = """${value as String} ${value is String} ${value in values}"""

        if (value is String) {
            for (item in values) {
                if (item !is String) continue
            }
            return lineTemplate
        }
        return parent
    }
}
