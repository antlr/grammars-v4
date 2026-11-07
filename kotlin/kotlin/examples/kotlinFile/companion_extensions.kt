// Tests companion extensions and the companion soft keyword.

package sample.companionextensions

companion
fun String.topLevel(): String = this
public companion fun <T> List<T>.firstValue(): T? = firstOrNull()

companion
val String.lengthFromCompanion: Int
    get() = length

private companion var MutableList<String>.lastText: String
    get() = last()
    set(value) { clear() }

@Extension companion inline fun Int.annotated(): Int = this

context(logger: Logger)
companion fun String.logged(): String = this

class Host {
    companion fun Int.member(): Int = this
    companion val String.memberProperty: Int get() = length
    companion object Named { fun regular() {} }
}

val companion = "soft keyword identifier"
fun companion() = "soft keyword function name"
val nestedInterpolation = "${ run { companion } }"
