// Tests parser errors for malformed collection literals.

package sample.badcollectionliterals

val leadingComma = [, 1]
val missingElement = [1,, 2]
val commaOnly = [,]
