# Swift 3

An ANTLR4 grammar for Swift 3 (Swift 3.0.1).

Based on:

* [Official documentation](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/zzSummaryOfTheGrammar.html) (it is incomplete)
* [Source code of swift compiler](https://github.com/apple/swift)
* Actual behavior

Notes:

* Swift compiler has support of some Swift 2 legacy in grammar
* Official documentation is incomplete (one of the examples - `#dsohandle` keyword)

Known issues and limitations:

* Does not work exactly as swift compiler, but designed to support pure Swift 3.0.1 syntax (without Swift 2 legacy)
* Contains workarounds, so the grammar can be changed (at the moment of this line is written there is `statements_impl` rule)
* It is not yet used on large projects (but I hope it will be changed)
* It is not well tested yet
* It can contain ambiguities or just wrong rules

About ambiguities, official documentation and aforementioned `statements_impl`.
There's Swift2 grammar here in this repo - it can not tell the difference between this:

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

Or this rule, it is just wrong:
```
operating_system : 'macOS' | 'iOS' | 'watchOS' | 'tvOS' ;
```

Source code of swift's stdlib contains code with "Android" and "Windows" as operating system.

If you want to contribute, please test the grammar carefully on the source 
code of Swift itself. See below.

## Contributing

### Making non-breaking changes

How-to:

* Clone this repo
* Ensure you have maven installed
* `cd grammars-v4/swift3`
* `mvn test`
* It will test the grammar with your changes.

If something goes wrong you can debug it.

Go to this folder:

* `cd grammars-v4/swift3/target/classes/`

Use one of the following commands (requires grun):

* `grun Swift3 top_level -gui -tree path/to/file.swift`
* `grun Swift3 the_rule_you_are_working_on -gui -tree path/to/file.swift`
* `grun Swift3 start -tokens path/to/file.swift`

I hope it will help. Anyway, it's fairly simple to figure out how it works 
even if you are new to antlr.

### Adding more tests

Beware of adding too much tests, swift parser is slow.
It can fail on Travis after you make a pull request due to current 
10min timeout per grammar and travis works very slow in comparison to your computer.

## Links

Permanent links for language reference:
[Swift 2.2](http://web.archive.org/web/20160423125921/https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/zzSummaryOfTheGrammar.html)
[Swift 3.0.1](http://web.archive.org/web/20170221054114/https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/zzSummaryOfTheGrammar.html).