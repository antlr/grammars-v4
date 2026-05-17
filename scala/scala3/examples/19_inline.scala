import scala.compiletime.erasedValue

object InlineDemo {
  inline def power(base: Double, inline exp: Int): Double = {
    if (exp == 0) 1.0
    else base * power(base, exp - 1)
  }

  inline def max(inline a: Int, inline b: Int): Int = {
    if (a > b) a else b
  }

  inline def typeName[T]: String = {
    inline erasedValue[T] match {
      case _: Int     => "Int"
      case _: String  => "String"
      case _: Boolean => "Boolean"
      case _          => "Unknown"
    }
  }

  def main(args: Array[String]): Unit = {
    println(power(2.0, 10))
    println(max(3, 7))
    println(typeName[Int])
    println(typeName[String])
  }
}
