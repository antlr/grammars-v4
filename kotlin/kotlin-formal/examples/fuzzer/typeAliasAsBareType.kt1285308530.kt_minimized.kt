typealias L = (List<T>)?
fun box(): (String)? {
val test: Collection<Int> = listOf(1, 2, 3)
if (test !is L) {
return "test !is L"
}
}