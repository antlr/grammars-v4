class X {
    var optional: X!
    var lazy: X!
    var dynamic: X!
    
    func keywordsAsVars(argument: String) {}
}
class Y: X {
    override func keywordsAsVars(argument: String) {
        optional.lazy.dynamic = X()
    }
}
