# TypeScript grammar

This TypeScript grammar does not exactly corresponds to TypeScript standard.
The main goal during developing was practical usage, performance and clarity
(getting rid of duplicates).

## Universal Actions & Semantic Predicates

Some modern TypeScript syntax can not be handled with standard context-free
grammars, for example detection of `get` keyword in getters and `get` identifiers
in other cases. Moreover, some parser options can be defined externally (`use strict`)
and should be considered during parsing process.

For such complex syntax [actions](https://github.com/antlr/antlr4/blob/master/doc/actions.md) and
[predicates](https://github.com/antlr/antlr4/blob/master/doc/predicates.md) are
used. This is a second grammar in repository with attempt to use an **universal**
actions and predicates. It works at least for **C#** and **Java** runtimes.

Consider the `getter` rule in grammar:

```ANTLR
getter
    : Identifier{p("get")}? propertyName
    ;
```

Instruction `p("get")` stands for *get the previous token value and return a boolean
value as a result of comparison to "get" string*.

For **Java** runtime it described by the following code in [Java/TypeScriptLexerBase.java](Java/TypeScriptLexerBase.java)

```Java
protected boolean prev(String str) {
    return _input.LT(-1).getText().equals(str);
}
```

For **C#** runtime by Sam Harwell it described by 
[CSharp/TypeScriptParserBase.cs](CSharp/TypeScriptParserBase.cs)

```CSharp
protected bool prev(string str)
{
    return _input.LT(-1).Text.Equals(str);
}
```

Furthermore the [`superClass`](https://github.com/antlr/antlr4/blob/master/doc/options.md)
option should be defined lexer and parser grammar files by the following manner:

```ANTLR
options {
    tokenVocab=TypeScriptLexer;
    superClass=TypeScriptBaseParser;
}
```

Runtimes super class names (`TypeScriptLexer`, `TypeScriptParser`) should be
the same for correct parser generation.

## Syntax support

Based on [JavaScript grammar](https://github.com/loonydev/grammars-v4/tree/master/javascript) by [Positive Technologies](https://github.com/PositiveTechnologies)

### TypeScript

See [examples](examples) directory with test data files.

## Main contributors

* Andrii Artiushok (2019) - initial version


## License

[MIT](https://opensource.org/licenses/MIT)