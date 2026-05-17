// Tests indentation-sensitive (colon) syntax — Scala 3 significant indentation.

// Class with indented body
class Point(val x: Int, val y: Int):
  def distanceTo(other: Point): Double =
    val dx = x - other.x
    val dy = y - other.y
    Math.sqrt(dx * dx + dy * dy)

  override def toString: String = s"Point($x, $y)"

// Object with indented body
object MathUtils:
  def max(a: Int, b: Int): Int =
    if a > b then a
    else b

  def factorial(n: Int): Int =
    if n <= 1 then 1
    else n * factorial(n - 1)

  def abs(n: Int): Int = if n >= 0 then n else -n

// Trait with indented body
trait Shape:
  def area: Double
  def perimeter: Double

// Class extending a trait with indented body
class Circle(val radius: Double) extends Shape:
  def area: Double = Math.PI * radius * radius
  def perimeter: Double = 2 * Math.PI * radius

// Indented match expression
def describe(x: Any): String = x match
  case i: Int    => s"Integer $i"
  case s: String => s"String '$s'"
  case _         => "Unknown"

// Indented for with yield
def squares(n: Int): List[Int] =
  for i <- 1 to n
  yield i * i

// Indented for with do
def printRange(n: Int): Unit =
  for i <- 1 to n
  do println(i)

// Extension with indented body (colon form)
extension (n: Int):
  def doubled: Int = n * 2
  def squared: Int = n * n

// Enum with indented body
enum Direction:
  case North
  case South
  case East
  case West

// Given with indented body (with keyword form)
trait Describable[A]:
  def describe(a: A): String

given Describable[Int] with
  def describe(n: Int): String = s"Int($n)"

// Nested indented blocks
class Tree(val value: Int, val children: List[Tree]):
  def sum: Int =
    val childSum = children.foldLeft(0)((acc, child) =>
      acc + child.sum
    )
    value + childSum

  def depth: Int =
    if children.isEmpty then 1
    else
      val maxChild = children.map(_.depth).max
      1 + maxChild
