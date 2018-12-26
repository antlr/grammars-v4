inline tailrec fun doCall(f: (() -> Unit)) = (f)!!()
fun test1(nonLocal: String): String {
val localResult = doCall({})
return ("NON_LOCAL_FAILED $localResult")!!
}