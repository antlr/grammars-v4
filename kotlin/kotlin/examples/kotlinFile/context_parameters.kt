// Tests context parameter declarations and modifiers.

package sample.contextparameters

@DeclarationAnnotation
context(logger: Logger)
public fun multilineFunction() {}

context(logger: Logger) fun sameLineFunction() {}

inline context(logger: Logger)
fun modifierOrderFunction() {}

context(@First logger: Logger, @Second other: @Type Other)
fun annotatedParameters() {}

@DeclarationAnnotation public context(logger: Logger)
val property: String = "value"

context(logger: Logger) val String.extension: String
    get() = this

context(logger: Logger) var mutable: String
    get() = ""
    set(value) {}

class ContextMembers {
    context(logger: Logger)
    val memberProperty: String = "member"

    context(logger: Logger)
    fun memberFunction() {}
}
