// Tests backing-field declarations in properties and setters.

package sample.backingfields

val inferredBackingField
    field = 1

val typedUninitializedBackingField: String
    field: String

val typedInitializedBackingField: List<String>
    field: List<String> = listOf("value")

class BackingFieldMembers {
    val inferredMember
        field = 2

    val typedMember: Int
        field: Int
}

var setterField: Int = 0
    set(value) {
        field = value
    }
