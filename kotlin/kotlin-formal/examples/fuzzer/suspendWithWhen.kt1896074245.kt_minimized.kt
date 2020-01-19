suspend fun suspendHere(): Int = suspendCoroutineOrReturn((if (true) {
({x -> })
} else {
({x -> })
}))