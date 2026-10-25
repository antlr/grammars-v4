open class SuperFoo {
public fun (Int)?.bar(): String {
if (this is Foo)({return baz()})
}
public operator fun Int.baz() = "OK"
}
class Foo: SuperFoo() {

}