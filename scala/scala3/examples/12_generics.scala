class Stack[A] {
  private var elements: List[A] = List.empty

  def push(elem: A): Unit = {
    elements = elem :: elements
  }

  def pop(): Option[A] = elements match {
    case Nil    => None
    case h :: t => elements = t; Some(h)
  }

  def peek: Option[A] = elements.headOption
  def isEmpty: Boolean = elements.isEmpty
  def size: Int = elements.length
}

def identity[A](x: A): A = x
def swap[A, B](pair: (A, B)): (B, A) = (pair._2, pair._1)

object GenericsDemo {
  def main(args: Array[String]): Unit = {
    val s = new Stack[Int]
    s.push(1)
    s.push(2)
    s.push(3)
    println(s.pop())
    println(s.peek)
    println(swap(("hello", 42)))
  }
}
