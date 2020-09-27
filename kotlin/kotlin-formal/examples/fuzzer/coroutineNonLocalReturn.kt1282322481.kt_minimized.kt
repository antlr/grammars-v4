suspend fun suspendHere(): String = suspendCoroutineOrReturn(((when {
true -> (if (true) {
({x -> x.resume("OK")
COROUTINE_SUSPENDED})
} else {
({x -> x.resume("OK")
COROUTINE_SUSPENDED})
})
else -> (if (true) {
({x -> x.resume("OK")
COROUTINE_SUSPENDED})
} else {
({x -> x.resume("OK")
COROUTINE_SUSPENDED})
})
})))