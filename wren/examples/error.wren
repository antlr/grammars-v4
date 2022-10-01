class Error {
  foreign static runtimeError
}

var fiber = Fiber.new {
  Error.runtimeError
}

var error = fiber.try()
System.print(error) // expect: Error!
System.print(fiber.isDone) // expect: true
System.print(fiber.error) // expect: Error!