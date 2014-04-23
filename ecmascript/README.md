# ECMAScript.g4

An ANTLR4 grammar for ECMAScript based on the
[Standard ECMA-262 5.1 Edition from June 2011](http://www.ecma-international.org/ecma-262/5.1).

This grammar has some embedded code inside its `@parser::members` and 
`@lexer::members` blocks in order to properly parse ECMA/JS source 
files.

## Parser

The parser needs to read certain tokens from the `HIDDEN` channel
for proper *semi colon insertion*. The following method signatures
are therefor embedded inside `@parser::members`:

```java
/**
 * Returns {@code true} iff on the current index of the parser's
 * token stream a token of the given {@code type} exists on the
 * {@code HIDDEN} channel.
 *
 * @param type
 *         the type of the token on the {@code HIDDEN} channel
 *         to check.
 *
 * ...
 */
private boolean here(final int type) { /* ... */ }

/**
 * Returns {@code true} iff on the current index of the parser's
 * token stream a token exists on the {@code HIDDEN} channel which
 * either is a line terminator, or is a multi line comment that
 * contains a line terminator.
 *
 * ...
 */
private boolean lineTerminatorAhead() { /* ... */ }
```

## Lexer

The lexer needs to determine whether to tokenize certain future reserved
words when operating in strict mode. It also needs to determine when 
tokenizing an octal integer when *not* operating in strict more. Finally,
the `/` can be, besides the division operator, possibly be part of a regex 
literal, which is checked inside `isRegexPossible()`:

```java
/**
 * Returns {@code true} iff the lexer operates in strict mode.
 *
 * @return {@code true} iff the lexer operates in strict mode.
 */
public boolean getStrictMode() { /* ... */ }

/**
 * Sets whether the lexer operates in strict mode or not.
 *
 * @param strictMode
 *         the flag indicating the lexer operates in strict mode or not.
 */
public void setStrictMode(boolean strictMode) { /* ... */ }

/**
 * Return the next token from the character stream and records this last
 * token in case it resides on the default channel. This recorded token
 * is used to determine when the lexer could possibly match a regex
 * literal.
 *
 * ...
 */
@Override
public Token nextToken() { /* ... */ }

/**
 * Returns {@code true} iff the lexer can match a regex literal.
 *
 * ...
 */
private boolean isRegexPossible() { /* ... */ }
```

# Porting

To port the grammar to another target, the embedded code outlined above
needs to be translated from Java to the new target language. When changing
any of the method names, be sure to rename these names inside the grammar 
rules as well.

# Unit tests

The grammar including lexer- and parser unit tests, as well as some example 
code of how to use it, can be found here: [github.com/bkiers/ecmascript-parser](https://github.com/bkiers/ecmascript-parser).

