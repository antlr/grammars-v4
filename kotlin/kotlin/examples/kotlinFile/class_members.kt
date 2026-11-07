// Tests class, interface, enum, and object member declarations.

package sample.classmembers

class CompactProperty { val value = 1 }

class CompactFunction { fun value() = 1 }

class SeparatedMembers {
    val first = 1
    fun second() = first
    var third = 3; val fourth = third
}

class NestedMembers {
    class Nested { val value = 1 }
    object NestedObject { val value = 2 }
    companion object { val value = 3 }
    init { val initialized = 4 }
    constructor() : this(0)
    constructor(value: Int)
}

interface CompactInterface { fun value() }

enum class CompactEnum {
    FIRST, SECOND;
    val value = 1
}

fun objectLiteralMember() = object { val value = 1 }
