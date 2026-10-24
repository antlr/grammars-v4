object ForDemo {
  def main(args: Array[String]): Unit = {
    val squares = for {
      i <- 1 to 5
    } yield i * i
    println(squares.toList)

    val pairs = for {
      x <- 1 to 4
      y <- 1 to 4
      if x != y
      if x + y == 5
    } yield (x, y)
    println(pairs.toList)

    val result: Option[Int] = for {
      a <- Some(3)
      b <- Some(4)
    } yield a + b
    println(result)
  }
}
