type StringOrInt = String | Int

def process(value: String | Int): String = value match {
  case s: String => s"String: $s"
  case n: Int    => s"Int: $n"
}

trait HasName {
  def name: String
}

trait HasAge {
  def age: Int
}

def greet(entity: HasName & HasAge): String = {
  s"Hello, ${entity.name}! You are ${entity.age} years old."
}

case class Person(name: String, age: Int) extends HasName with HasAge

object UnionDemo {
  def main(args: Array[String]): Unit = {
    println(process("hello"))
    println(process(42))
    println(greet(Person("Alice", 30)))
  }
}
