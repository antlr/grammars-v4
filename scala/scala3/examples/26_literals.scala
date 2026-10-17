// Covers: CharacterLiteral, SymbolLiteral, NullLiteral

object LiteralsDemo {
  val ch1: Char = 'a'
  val ch2: Char = '\n'
  val ch3: Char = '\t'

  val sym1 = 'hello
  val sym2 = 'world

  val n1: String = null
  val n2: Any = null

  def charFun(c: Char): Int = c.toInt

  def nullCheck(s: String): Boolean = s == null

  val r1: Int = charFun('z')
  val r2: Boolean = nullCheck(null)
}
