suspend fun suspendWithValue(result: (() -> (String)?)): String = suspendCoroutineOrReturn((if (true) {
({x -> })
} else {
({x -> })
}))