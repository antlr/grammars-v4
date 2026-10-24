import scala.util.{Try, Success, Failure}

object ErrorHandling {
  def safeDivide(a: Int, b: Int): Option[Int] = {
    if (b == 0) None else Some(a / b)
  }

  def parseAge(s: String): Either[String, Int] = {
    s.toIntOption match {
      case None    => Left(s"'$s' is not a valid integer")
      case Some(n) =>
        if (n < 0 || n > 150) Left(s"$n is not a valid age")
        else Right(n)
    }
  }

  def main(args: Array[String]): Unit = {
    val result = for {
      r  <- safeDivide(10, 2)
      r2 <- safeDivide(r, 2)
    } yield r2
    println(result)

    println(parseAge("25"))
    println(parseAge("abc"))

    val t = Try(10 / 0)
    t match {
      case Success(v) => println(s"Got $v")
      case Failure(e) => println(s"Error: ${e.getMessage}")
    }
  }
}
