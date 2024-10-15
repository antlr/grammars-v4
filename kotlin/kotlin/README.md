# Kotlin ANTLR4 grammar

ANTLR4 grammar for Kotlin written only in ANTLR's special syntax.

## Links
* [EBNF Kotlin grammar](http://kotlinlang.org/docs/reference/grammar.html)
* [Kotlin specification](https://kotlinlang.org/spec)

## License
Licensed under the Apache 2.0

## Testing
Test.kt includes the test data from the [JetBrains's repository](https://github.com/JetBrains/kotlin/tree/master/compiler/testData/psi).

## Contacts
Anastasiya Shadrina a.shadrina5@mail.ru

## Origin source
<https://github.com/shadrina/kotlin-grammar-antlr4>

## Issues
The grammar has ambiguities in:
```
callSuffix
callableReference
classBody
classDeclaration
classMemberDeclaration
controlStructureBody
delegationSpecifier
delegationSpecifiers
enumEntries
functionDeclaration
functionLiteral
ifExpression
importHeader
jumpExpression
kotlinFile
modifier
objectLiteral
postfixUnaryExpression
propertyDeclaration
receiverType
secondaryConstructor
semi
simpleUserType
statement
statements
superExpression
type
typeRHS
typeReference
userType
valueArgument
whenEntry
whenExpression
```
