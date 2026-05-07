type Elem[X] = X match {
  case String      => Char
  case Array[t]    => t
  case Iterable[t] => t
}

type LeafElem[X] = X match {
  case String      => Char
  case Array[t]    => LeafElem[t]
  case Iterable[t] => LeafElem[t]
  case AnyVal      => X
}

object MatchTypeDemo {
  def main(args: Array[String]): Unit = {
    val t = ("hello", 42, true)
    println(t)
    println(t._1.length)
  }
}
