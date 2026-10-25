class Point(val x: Double, val y: Double) {
  def distanceTo(other: Point): Double = {
    val dx = x - other.x
    val dy = y - other.y
    Math.sqrt(dx * dx + dy * dy)
  }
  override def toString: String = s"Point($x, $y)"
}

object Point {
  def origin: Point = new Point(0.0, 0.0)
}
