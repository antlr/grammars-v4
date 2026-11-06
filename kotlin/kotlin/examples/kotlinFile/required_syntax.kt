package samples.requiredsyntax

enum class Empty {}
enum class Single { ONE }
enum class Values {
    FIRST,
    SECOND(1) { fun marker() {} },
    THIRD,
    ;
    fun helper() {}
}
enum class Numbered(val value: Int) { ONE(1), TWO(2), }

fun handlers() {
    try {} catch (first: Exception) {}
    try {} finally {}
    try {} catch (first: Exception) {} catch (second: RuntimeException) {} finally {}
}

fun calls() {
    target {}
    target() {}
    target<String> {}
    target()() {}
    target() {}()
    target[0] {}
    target label@ {}
    target @Suppress("unused") {}
    target() {}.invoke {}
}
