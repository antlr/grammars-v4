trait Producer[+A] {
  def produce: A
}

trait Consumer[-A] {
  def consume(a: A): Unit
}

class Box[+A](val value: A) extends Producer[A] {
  def produce: A = value
  def map[B](f: A => B): Box[B] = new Box(f(value))
}

class Printer[-A](show: A => String) extends Consumer[A] {
  def consume(a: A): Unit = println(show(a))
}

object VarianceDemo {
  def printProducer[A](p: Producer[A]): Unit = println(p.produce)

  def main(args: Array[String]): Unit = {
    val intBox: Box[Int] = new Box(42)
    val anyBox: Box[Any] = intBox
    printProducer(anyBox)

    val anyPrinter: Printer[Any] = new Printer(_.toString)
    val intPrinter: Printer[Int] = anyPrinter
    intPrinter.consume(42)
  }
}
