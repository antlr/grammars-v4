# Swift 5

An ANTLR4 grammar for Swift 5 (Swift 5.4).

Based on:

* [Official documentation](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/zzSummaryOfTheGrammar.html) (it is incomplete)
* [Source code of swift compiler](https://github.com/apple/swift)
* [ANTLR4 grammar for Swift 3](https://github.com/antlr/grammars-v4/tree/master/swift/swift3)
* Actual behavior

Notes:

* Official documentation is incomplete

Known issues and limitations:

* Does not work exactly as swift compiler, but designed to support pure Swift 5.4 syntax
* Contains workarounds for context dependent rules and mutually left-recursive rules, so the grammar can be changed (at the moment this line is written, there is a `statements_impl` rule and a `inner_postfix_expression` rule)
* It is not yet used on large projects
* It is not well tested yet
* It can contain ambiguities or wrong rules

About ambiguities, official documentation and aforementioned `statements_impl`.
It can not tell the difference between this:

```Swift
func x() -> Int { return 1 }
let y = x()
print(y) // prints "1"
```

and this:

```Swift
func x() -> Int { return 1 }
let y = x
()
print(y) // prints "(Function)"
```

Yes, it is somehow documented, but it is not a part of grammar.

If you want to contribute, please test the grammar carefully on the source 
code of Swift itself. See below.

## Contributing

### Making non-breaking changes

How-to:

* Clone this repo
* Ensure you have maven installed
* `cd grammars-v4/swift5`
* `mvn test`
* It will test the grammar with your changes.

If something goes wrong you can debug it.

Go to this folder:

* `cd grammars-v4/swift3/target/classes/`

Use one of the following commands (requires grun):

* `grun Swift5 top_level -gui -tree path/to/file.swift`
* `grun Swift5 the_rule_you_are_working_on -gui -tree path/to/file.swift`
* `grun Swift5 start -tokens path/to/file.swift`