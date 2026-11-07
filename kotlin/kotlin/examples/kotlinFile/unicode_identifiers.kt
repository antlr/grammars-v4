// Tests Unicode identifiers in declarations and string templates.

package sample.unicodeidentifiers

private fun unicodeNames(𞤀: String) {
    val Ა = 𞤀
    val 𐐀 = Ა
    val digit١ = 𐐀
    println("$Ა:$𐐀:${𞤀}")
}
