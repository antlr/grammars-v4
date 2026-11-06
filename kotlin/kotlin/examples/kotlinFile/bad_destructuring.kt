fun invalidDestructuring(values: Pair<String, Int>) {
    (val name, age) = values
    (val missingInitializer, val anotherMissingInitializer)
}