# The optimized Java grammar

This grammar, based on the optimized Java7 grammar by Terence Parr and Sam Harwell,
is optimized for performance, practical usage, and clarity. It does not correspond
exactly to the [Java Language Specification](https://docs.oracle.com/javase/specs/).
The [java8](../java8), [java9](../java9), and [java20](../java20) grammars
follow the JLS, but are slower that this grammar due to ambiguity and max-k problems
in the published JLS EBNF.

This grammar parses the file [ManyStringsConcat.java](examples/ManyStringsConcat.java)
faster than the original grammar without left recursion for expressions.

## Supported Java version
* Java 24 (latest)

## Main contributors
* Terence Parr, 2013
* Sam Harwell, 2013
* Ivan Kochurkin ([Positive Technologies](https://github.com/PositiveTechnologies)), 2017
* Michał Lorek, 2021

## Tests
* See examples/
* OpenJDK 24, `src/**/*.java` (using [Trash trgen to create app](https://github.com/kaby76/Trash/tree/main/src/trgen), then `find ~/jdk-jdk-23-ga/src/ -name '*.java' | cygpath -w -f - | ./Test -x`)

## Benchmarks
Grammar performance has been tested on the following Java projects:
* [OpenJDK 24](https://github.com/openjdk/jdk/archive/refs/tags/jdk-24-ga.zip)
* Spring Framework
* Elasticsearch
* RxJava
* JUnit4
* Guava
* Log4j

See the [benchmarks](Benchmarks.md) page for details.

## Grammar style
Please use [antlr-format](https://github.com/antlr-ng/antlr-format) and
[formatting style config](https://github.com/antlr/grammars-v4/blob/master/_scripts/repo_coding_style.json)
to reformat in the [coding standard format for the repo](https://github.com/antlr/grammars-v4/wiki#is-there-a-coding-standard-for-antlr4-grammars).

### String literals
Generally, you can use either a string literal or the corresponding lexer rule name
(`TOKEN_REF`) directly in a parser rule for a token. It makes no difference because the
[java/java/ grammar](https://github.com/antlr/grammars-v4/tree/master/java/java)
is a split Antlr4 grammar, and the Antlr Tool prevents you from defining a token using
a string literal in a parser rule (it outputs
`cannot create implicit token for string literal in non-combined grammar` if you try).
When writing an Antlr listener or visitor, use the corresponding lexer rule name for the
string literal used in the parser rule.

Currently, the grammar contains a mixture of string literals
and lexer rule names in parser rules. If you want a parser grammar that removes all string literals
from parser rules, use [Trash trfoldlit](https://github.com/kaby76/Trash/tree/main/src/trfoldlit).
If you want a parser grammar that uses string literals where a lexer rule exists for the string
literal, use [Trash trunfoldlit](https://github.com/kaby76/Trash/tree/main/src/trunfoldlit).
