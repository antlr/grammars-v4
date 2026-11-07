// Tests expect and actual modifiers and identifiers.

package sample.multiplatformmodifiers

expect class CommonApi
actual class PlatformApi

expect fun apiName(): String
actual fun apiName(): String = "jvm"

expect val platformName: String
actual val platformName: String = "jvm"

expect object CommonObject
actual object PlatformObject

expect typealias CommonName = String
actual typealias PlatformName = String

public expect
fun visibleApi()

@Marker actual public fun annotatedApi(): Int = 0

actual class WithConstructor {
    actual constructor() {}
}

actual class WithPrimaryConstructor actual constructor()

val expect = 1
val actual = expect
fun names(expect: Int, actual: Int) = expect + actual
val expectation = actual
val actualized = expectation
val `expect` = `actual`
