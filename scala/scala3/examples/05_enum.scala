enum Color {
  case Red, Green, Blue
}

enum Direction {
  case North, South, East, West

  def opposite: Direction = this match {
    case North => South
    case South => North
    case East  => West
    case West  => East
  }
}

enum Shape {
  case Circle(radius: Double)
  case Rectangle(width: Double, height: Double)

  def area: Double = this match {
    case Circle(r)       => Math.PI * r * r
    case Rectangle(w, h) => w * h
  }
}
