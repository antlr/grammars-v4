// Tests value class declarations and modifier placement.

package sample.valueclasses

value class UserId(val value: Int)

value class Box<T>(val value: T)

@Marker value class AnnotatedId(val value: Int)

expect value class CommonId
actual value class PlatformId(val value: Int)

value
class NewlineId(val value: Int)

class Outer {
    value class Nested(val value: Int)
}

val value = UserId(1)
val valueClass = value.value
