class Test {
  static run() {
    var root = Fiber.current
    System.print("begin root") // expect: begin root

    Fiber.new {
      System.print("in new fiber") // expect: in new fiber
      root.call() // expect runtime error: Cannot call root fiber.
      System.print("called root")
    }.transfer()
  }
}