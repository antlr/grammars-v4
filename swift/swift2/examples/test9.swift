class TestClass {
    var x = 0.0, y = 0.0

    // Subscript Syntax
    subscript(index: Int) -> Int {
        get {
            // return an appropriate subscript value here
        }
        set(newValue) {
            // perform a suitable setting action here
        }
    }

    // Closure Expression Syntax
    var reversed = names.sort({ (s1: String, s2: String) -> Bool in
        return s1 > s2
    })

    func backwards(s1: String, _ s2: String) -> Bool {
        return s1 > s2
    }

    // Inferring Type From Context
    var reversed = names.sort( { s1, s2 in return s1 > s2 } )

    // Implicit Returns from Single-Expression Closures
    var reversed = names.sort( { s1, s2 in s1 > s2 } )

    // Shorthand Argument Names
    var reversed = names.sort( { $0 > $1 } )

    let strings = numbers.map {
        (var number) -> String in
        var output = ""
        while number > 0 {
            output = digitNames[number % 10]! + output
            number /= 10
        }
        return output
    }

    // Capturing Values
    func makeIncrementer(forIncrement amount: Int) -> () -> Int {
        var runningTotal = 0
        func incrementer() -> Int {
            runningTotal += amount
            return runningTotal
        }
        return incrementer
    }

    // Nonescaping Closures
    var completionHandlers: [() -> Void] = []
    func someFunctionWithEscapingClosure(completionHandler: () -> Void) {
        completionHandlers.append(completionHandler)
    }

    // Generic Functions
    func swapTwoValues<T>(inout a: T, inout _ b: T) {
        let temporaryA = a
        a = b
        b = temporaryA
    }

    // Calling generic functions with parameters
    var someInt = 3
    var anotherInt = 107

    func foo() { swapTwoValues(&someInt, &anotherInt) }

    // someInt is now 107, and anotherInt is now 3

    var someString = "hello"
    var anotherString = "world"

    // Type Constraint Syntax
    func someFunction<T: SomeClass, U: SomeProtocol>(someT: T, someU: U) {
        // function body goes here
    }

    // Type constraints in action
    func findStringIndex(array: [String], _ valueToFind: String) -> Int? {
        for (index, value) in array.enumerate() {
            if value == valueToFind {
                return index
            }
        }
        return nil
    }

    func findIndex<T>(array: [T], _ valueToFind: T) -> Int? {
        for (index, value) in array.enumerate() {
            if value == valueToFind {
                return index
            }
        }
        return nil
    }

    // Where clauses
    func allItemsMatch<
        C1: Container, C2: Container
        where C1.ItemType == C2.ItemType, C1.ItemType: Equatable>
        (someContainer: C1, _ anotherContainer: C2) -> Bool {

            // check that both containers contain the same number of items
            if someContainer.count != anotherContainer.count {
                return false
            }

            // check each pair of items to see if they are equivalent
            for i in 0..<someContainer.count {
                if someContainer[i] != anotherContainer[i] {
                    return false
                }
            }

            // all items match, so return true
            return true

    }

    // Bitwise NOT Operator
    let initialBits: UInt8 = 0b00001111
    let invertedBits = ~initialBits  // equals 11110000

    // Bitwise AND Operator
    let firstSixBits: UInt8 = 0b11111100
    let lastSixBits: UInt8  = 0b00111111
    let middleFourBits = firstSixBits & lastSixBits  // equals 00111100

    // Bitwise OR Operator
    let someBits: UInt8 = 0b10110010
    let moreBits: UInt8 = 0b01011110
    let combinedbits = someBits | moreBits  // equals 11111110

    // Bitwise XOR Operator
    let firstBits: UInt8 = 0b00010100
    let otherBits: UInt8 = 0b00000101
    let outputBits = firstBits ^ otherBits  // equals 00010001

    // Shifting Behavior for Unsigned Integers
    let shiftBits: UInt8 = 4   // 00000100 in binary
    func foo() {
	    shiftBits << 1             // 00001000
	    shiftBits << 2             // 00010000
	    shiftBits << 5             // 10000000
	    shiftBits << 6             // 00000000
	    shiftBits >> 2             // 00000001
    }

    // Overflow Operators
    var unsignedOverflow = UInt8.max
    // unsignedOverflow equals 255, which is the maximum value a UInt8 can hold
    func foo() {
    	unsignedOverflow = unsignedOverflow &+ 1
        // unsignedOverflow equals 0, which is the minimum value a UInt8 can hold
        unsignedOverflow = unsignedOverflow &- 1
    }

    var unsignedOverflow = UInt8.min
    // unsignedOverflow is now equal to 255

    func precedenceAndAssociativity() -> Void {
        2 + 3 % 4 * 5 // equals 17
    }

    // Prefix Operators
    prefix func - (vector: Vector2D) -> Vector2D {
        return Vector2D(x: -vector.x, y: -vector.y)
    }

    // Compount assignment operators
    func += (inout left: Vector2D, right: Vector2D) {
        left = left + right
    }
}
