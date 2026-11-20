// Tests collection literals in declarations, calls, loops, and destructuring.

package sample.collectionliterals

@Suppress(["unused",])
fun collectionLiteralCases(values: List<Int>): List<Int> {
    val empty: List<Int> = []
    val singleton = [1,]
    val nested = [[1, 2,], [],]
    val argument = consume([3, 4,])
    val multiline = [
        1,
        2,
    ]
    val indexed = [10, 20,][0]
    val [first, second] = [30, 40,]

    for (item in [1, 2,]) {
        values.forEach { [item,] }
    }

    return [empty.size, singleton.size, nested.size, multiline.size, indexed, first, second,]
}
