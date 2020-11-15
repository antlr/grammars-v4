class ArrayWrapper<T>() {
val contents = ArrayList<T>()
operator inline suspend fun plus(b: ArrayWrapper<T>): ArrayWrapper<T> {
val result = ArrayWrapper<T>()
result.contents.addAll(b.contents)
return result
}
}
suspend fun box(): String {
val v1 = ArrayWrapper<String>()
val v2 = ArrayWrapper<String>()
val v3 = v1 + v2
return if (v3.contents.size == 2) {
"OK"
} else {
"fail"
}
}