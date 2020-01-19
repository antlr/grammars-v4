suspend fun foo(value: String): (String)? = suspendCoroutineOrReturn((if (true) {
({x -> })
} else {
({x -> })
}))