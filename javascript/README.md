# JavaScript grammar by [Positive Technologies](https://github.com/PositiveTechnologies)

This JavaScript grammar does not exactly corresponds to ECMAScript standard.
The main goal during developing was practical usage, performance and clarity
(getting rid of duplicates).

## Universal Actions & Semantic Predicates

Some modern JavaScript syntax can not be handled with standard context-free
grammars, for example detection of `get` keyword in getters and `get` identifiers
in other cases. Moreover, some parser options can be defined externally (`use strict`)
and should be considered during parsing process.

For such complex syntax [actions](https://github.com/antlr/antlr4/blob/master/doc/actions.md) and
[predicates](https://github.com/antlr/antlr4/blob/master/doc/predicates.md) are
used. This is a first grammar in repository with attempt to use an **universal**
actions and predicates. It works at least for **C#** and **Java** runtimes.

Consider the `getter` rule in grammar:

```ANTLR
getter
    : Identifier{p("get")}? propertyName
    ;
```

Instruction `p("get")` stands for *get the previous token value and return a boolean
value as a result of comparison to "get" string*.

For **Java** runtime it described by the following code in [Java/JavaScriptBaseLexer.java](Java/JavaScriptBaseParser.java)

```Java
protected boolean prev(String str) {
    return _input.LT(-1).getText().equals(str);
}
```

For **C#** runtime by Sam Harwell it described by 
[CSharpSharwell/JavaScriptBaseParser.cs](CSharpSharwell/JavaScriptBaseParser.cs)

```CSharp
protected bool prev(string str)
{
    return _input.Lt(-1).Text.Equals(str);
}
```

Furthermore the [`superClass`](https://github.com/antlr/antlr4/blob/master/doc/options.md)
option should be defined lexer and parser grammar files by the following manner:

```ANTLR
options {
    tokenVocab=JavaScriptLexer;
    superClass=JavaScriptBaseParser;
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
* ~~Generators~~
* Internationalization & Localization
* ~~Iterators~~ (not supported for now)
* Map/Set& WeakMap/WeakSet
* Meta-Programming
* ~~Modules~~ (partial support)
* New Build-In Methods
* Promises
* Scoping
* Strict Functions
* Strict Global
* Symbol Type
* Template Literals
* Typed Arrays

See [examples](examples) directory with test data files.

### Outdated

Also this grammar supports outdated syntax such as

* Html Comment
* CData section

## Main contributors

* Bart Kiers (2014) - initial version
* Ivan Kochurkin (2017):
  * Updated for EcmaScript 6 support
  * Cleared & optimized
  * Universal code actions & predicates
  * Support of some outdated syntax (Html comment, CData)

## License

[MIT](https://opensource.org/licenses/MIT)