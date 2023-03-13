fun f2(): Array? {
    return null
}

fun foo() =
    when (val type = f2()) {
        is Array -> {
            1
        }

        else -> {
            0
        }
    }
