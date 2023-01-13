interface MComparator<T> {
fun compare(o1: T, o2: T): Int
}
open class CustomerService {
inline fun <T> comparator(z: (() -> Int)) = object: MComparator<T>{
override fun compare(o1: T, o2: T): Int {
return (z)!!()
}
}
fun callInline() = comparator<String>({1})
}