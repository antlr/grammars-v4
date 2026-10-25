// Integer
let _ = 0
let _ = 1
let _ = 1_000
let _ = 0x000
let _ = 0b000

// Floating-point
let _ = 1.0
let _ = 1.0e+1
let _ = 1e-1

// Boolean
let _ = true

// String
let _ = "Hello, world!"
let _ = "Hello, \(1)!"
let _ = "Hello, \(1.0e+1)!"
let _ = "Hello, \(Int.max)!"

// Nil
if (nil == nil) {}

// Array
let _: [Int] = []
let _ = [1, 2, 3]
let _: [Any] = [
    0, 1, 1.0, 1.0e+1, 1e+1, true,
    "Hello, world!", "Hello, \(1)!", "Hello, \(1.0e+1)!", "Hello, \(Int.max)!",
    (nil == nil)
]

// Dictionary
let _: [Int: Int] = [:]
let _: [Int: Any] = [
    0: [1, 1.0, 1.0e+1, 1e+1, true],
    1: ["Hello, world!", "Hello, \(1)!", "Hello, \(1.0e+1)!", "Hello, \(Int.max)!"],
    2: [(nil == nil)]
]
