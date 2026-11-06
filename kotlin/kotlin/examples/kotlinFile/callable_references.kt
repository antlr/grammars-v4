fun refs() {
    val callReceiver = factory()::run
    val parenthesizedReceiver = (value)::member
    val thisReceiver = this::member
    val classReceiver = factory()::class
    val labeledThisReceiver = this@refs::member
    val indexedReceiver = values[0]::member
    val nonNullReceiver = nullable!!::member
    val navigatedReceiver = value.member::nested
    val escapedTarget = factory()::`when`
    val softKeywordTarget = factory()::inline

    val referenceThenCall = factory()::run.invoke()
    val referenceThenNavigation = factory()::run.member
    val referenceThenSafeNavigation = factory()::run?.member
    val parenthesizedReferenceThenCall = (value)::member.invoke()

    val receiverless = ::factory
    val typeReceiver = String::length
    val genericTypeReceiver = List<String>::size
    val nullableTypeReceiver = String?::length
    val thisClass = this::class
}
