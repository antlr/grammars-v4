protocol Initializable {
    init()
}

protocol SelfTest: Initializable, Equatable {
    associatedtype Other: Initializable, Equatable
    subscript (_: Self) -> Self { get set }
}

//self_expression
// : 'self'
// | 'self' '.' declaration_identifier
// | 'self' '[' expression_list ']'
// | 'self' '.' 'init'
// | 'Self' // Self()
// | 'Self' '.' declaration_identifier // Self.This()
// | 'Self' '.' 'init' // Self.init()
// ;

extension SelfTest {
    func x() -> Bool {
        return self == self
            && self.x() == self.x()
            && self == self[self]
            && self == Self()
            && self == Self.init()
            && Self.Other() == Self.Other()
    }
}

class SelfClass {
    init() {
    }
    convenience init(_: SelfClass) {
        self.init()
    }
}
