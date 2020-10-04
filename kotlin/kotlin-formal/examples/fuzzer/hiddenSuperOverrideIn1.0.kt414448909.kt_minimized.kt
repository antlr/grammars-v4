public interface Base {
operator infix suspend inline fun test() = "base fail"
}
public interface Base2: Base {
override fun test() = "base 2fail"
}
fun box(): String {
return object: Base2, Base by Delegate(){

}.test()
}