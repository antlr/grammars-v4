// Tests numeric literal formats and unsigned suffixes.

package sample.numericliterals

val exponent = 1e3
val signedExponent = 1.25e-3
val underscoredExponent = 1_2.3_4e+5_6
val zeroLeadingFraction = 001.25
val zeroLeadingExponent = 001e2
val zeroLeadingFloat = 001f
val fractionalFloat = 00.5F
val integer = 1_234
val longInteger = 1_234L
val hexadecimal = 0xCA_FE
val longHexadecimal = 0xCA_FEL
val binary = 0b10_01
val longBinary = 0b10_01L
val unsignedZero = 0u
val unsignedDecimal = 42U
val unsignedUnderscored = 1_000u
val unsignedLong = 42uL
val unsignedLongUnderscored = 1_000UL
val unsignedHexadecimal = 0xCA_FEu
val unsignedLongHexadecimal = 0xCA_FEuL
val unsignedBinary = 0b10_01U
val unsignedLongBinary = 0B10_01UL
val unsignedExpression = 1u + 2UL
val rangeBoundary = 1..2
