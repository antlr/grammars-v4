import UIKit

prefix operator ^
postfix operator ^
infix operator ^
prefix operator √

public class OperatorTest {
    let testString: String = "hey"
    var subtractionTest = 100 - 5
    let multiplicationTest = 10 * 9
    var additionTest = 10 + 10
    var moduleTest = 10 % 10
    var gtTest = 5 > 1
    var ltTest = 5 < 10
    var not = !(5 > 10)
    var x: Int? = 0
    var y = 1
    var (z, _) = (10, 20)
    var (a, _, (b, c)) = ("test", 9.45, (12, 3))
    let bat = "BAT"
    let man = "MAN"
    
    
    func increment() -> Void {
        x! += 1
        x! -= 1
        y += 1
        y -= 1
        
        // String interpolation
        "\(bat)\(man)"
    }
}
