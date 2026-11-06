package samples.whenguards

fun guarded(value: Any, enabled: Boolean) = when (value) {
    is String if value.isNotEmpty() -> value
    in listOf(1, 2) if enabled -> "member"
    1, 2 if enabled && value != null -> "values"
    else if enabled -> "fallback"
    else -> "default"
}

fun nestedGuard(value: Any) = when (value) {
    is String if if (value.isEmpty()) false else true -> "nested"
    else -> "other"
}

fun subjectVariable(value: Int) = when (val subject = value) {
    is Int if subject > 0 -> "positive"
    else -> "non-positive"
}

fun template(value: Any) = "${when (value) {
    is String if value.isNotEmpty() -> value
    else -> "other"
}}"