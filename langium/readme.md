# Langium Grammar

## Author
Ken Domino

## License
MIT

## Source
This grammar is derived from the Langium grammar at https://github.com/eclipse-langium/langium/blob/7a0216638ba9d0869739b3370d50b718eaa036ee/packages/langium/src/grammar/langium-grammar.langium

Langium syntax is roughly equivalent to that for Antlr4. The Antlr4 grammar has no tree
rule labels because the use of labels is archaic. In Langium
the equivalent of a label is called a "property", which--surprisingly--is required because
children of the tree node must be explicitly named.

The regular expression
rules are roughly equivalent to that for JavaScript, but I cannot guarentee that it is
completely correct. The use of the semantic predicate for RegexLiteral is to make sure
comments are not lexed as regular expressions.

## References and Links

* [Langium source](https://github.com/eclipse-langium/langium/tree/main)
* [langium-sql](https://github.com/TypeFox/langium-sql)

