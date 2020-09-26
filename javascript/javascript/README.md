# JavaScript Grammar

This JavaScript grammar does not exactly corresponds to ECMAScript standard.
The main goal during developing was practical usage, performance and clarity
(getting rid of duplicates).

## Universal Actions & Semantic Predicates

Some modern JavaScript syntax can not be handled with standard context-free
grammars, for example detection of `get` keyword in getters and `get` identifiers
in other cases. Moreover, some parser options can be defined externally (`use strict`)
and should be considered during parsing process.

For such complex syntax [actions](https://github.com/antlr/antlr4/blob/master/doc/actions.md)
and [predicates](https://github.com/antlr/antlr4/blob/master/doc/predicates.md)
are used. This is a first grammar in repository with attempt to use an **universal**
actions and predicates. It works at least for **C#** and **Java** runtimes.

Consider the `getter` rule in grammar:

```g4
getter
    : Identifier{p("get")}? propertyName
    ;
```

Instruction `p("get")` stands for *get the previous token value and return a boolean
value as a result of comparison to "get" string*.

For **Java** runtime it described by the following code in [Java/JavaScriptLexerBase.java](Java/JavaScriptParserBase.java)

```java
protected boolean prev(String str) {
    return _input.LT(-1).getText().equals(str);
}
```

For **C#** runtime by Sam Harwell it described by 
[CSharp/JavaScriptParserBase.cs](CSharp/JavaScriptParserBase.cs)

```cs
protected bool prev(string str)
{
    return _input.Lt(-1).Text.Equals(str);
}
```

Furthermore the [`superClass`](https://github.com/antlr/antlr4/blob/master/doc/options.md)
option should be defined lexer and parser grammar files by the following manner:

```g4
options {
    tokenVocab=JavaScriptLexer;
    superClass=JavaScriptParserBase;
}
```

Runtimes super class names (`JavaScriptLexer`, `JavaScriptParser`) should be
the same for correct parser generation.

## Syntax support

### ECMAScript 6

Grammar supports the following list of ECMAScript 6 features taken from
<http://es6-features.org>:

* Arrow Functions
* Classes
* Constants
* Destructuring Assignment
* Enhanced Object Properties
* Enhanced Regular Expression
* Extended Literals
* Extended Parameter Handling
* Generators
* Internationalization & Localization
* Iterators
* Map/Set& WeakMap/WeakSet
* Meta-Programming
* Modules
* New Build-In Methods
* Promises
* Scoping
* Strict Functions
* Strict Global
* Symbol Type
* Template Literals
* Typed Arrays

See [examples](examples) directory with test data files.

### ES6 to ES2020

* HashBang Comment
* `**` and `**=`
* Numeric Literal Seprator (`1_23`)
* BigInt (`123456n`)
* Async Await 
* Async Iteration (`for await`)
* Dynamic Import (`import()`)
* Private Field (`#field`)
* Null Coalesce (`a??b`)
* Optional Chain (`a?.b`)
* Calculated Property (`[name]:value`)

### Outdated

Also this grammar supports outdated syntax such as

* Html Comment
* CData section

## Main contributors

* Bart Kiers (2014) - initial version
* Ivan Kochurkin (2017, Positive Technologies):
  * Updated for EcmaScript 6 support
  * Cleared & optimized
  * Universal code actions & predicates
  * Support of some outdated syntax (Html comment, CData)
* Student Main (2019):
  * Update to ES2020

## Running fuzz test

1. You need recent Node.js
2. `npm i -g eslump`
3. `generate.bat` (For linux: manually run them....)
4. `fuzztest.bat` (Or: `eslump wrapper.js gen/`)
5. Ctrl-C to stop

Error will show in terminal, correspond code located in `gen/temp.js`

## License

[MIT](https://opensource.org/licenses/MIT)