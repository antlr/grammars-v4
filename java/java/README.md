# The optimized Java grammar

This grammar, based on the optimized Java7 grammar by Terence Parr and Sam Harwell,
is meant to parse the latest for the Java language, and is optimized for
performance, practical usage, and clarity.

It does not correspond
exactly to the [Java Language Specification](https://docs.oracle.com/javase/specs/).
The [java8](../java8), [java9](../java9), and [java20](../java20) grammars
follow the JLS, but are slower that this grammar due to ambiguity and max-k problems
in the published JLS EBNF.

This grammar parses the file [ManyStringsConcat.java](examples/ManyStringsConcat.java)
faster than the unoptimized java grammars. It implements operator precedence
using Antlr4-style alt ordering instead of operator-precedence rules. Thus, it avoids
creating parse trees with long, single-child chains for each string literal constant in
[ManyStringsConcat.java](examples/ManyStringsConcat.java). In addition, it is faster
because it avoids the large ATN-config set construction in the
`AdaptivePredict()` parsing engine.

[Java Enhancement Proposals (JEP)](https://openjdk.org/jeps/0)
are not implemented in this grammar.

## Currently supported Java version
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

## Performance Summary (N=3 runs, mean ± SEM)
This is the performance of the target parse against the JDK SDK 21 source.

| Grammar | PT (s) | OT (s) | TT (s) | TPS | Post-warmup TPS | Post-warmup Speed Up |
|---------|--------|--------|--------|-----|-----------------|----------------------|
| java/java,Cpp | 282 ± 1.755 | 83.2 ± 53.6 | 365.2 ± 55.03 | 7.865e+04 ± 491.5 | 7.866e+04 ± 492.4 | 2.04 ± 0.08021 |
| java/java,CSharp | 176.6 ± 0.6581 | 4.654 ± 0.09829 | 181.2 ± 0.6459 | 1.256e+05 ± 468.6 | 1.257e+05 ± 467.7 | 9.373 ± 0.07513 |
| java/java,Dart | 200 ± 0.5003 | 16.52 ± 0.04388 | 216.5 ± 0.4851 | 1.109e+05 ± 277.3 | 1.109e+05 ± 277 | 2.283 ± 0.01764 |
| java/java,Go | 358.4 ± 0.6414 | 21.72 ± 12.37 | 380.1 ± 11.73 | 6.189e+04 ± 111 | 6.19e+04 ± 111 | 2.047 ± 0.006667 |
| java/java,Java | 108.7 ± 0.648 | 7.631 ± 0.02434 | 116.3 ± 0.6287 | 2.041e+05 ± 1222 | 2.045e+05 ± 1228 | 16.17 ± 0.4402 |
| java/java,Rust | 315.9 ± 0.3973 | 15.37 ± 0.00809 | 331.3 ± 0.3907 | 7.02e+04 ± 88.12 | 7.021e+04 ± 87.85 | 2.027 ± 0.05364 |
| java/java,TypeScript | 916.2 ± 2.695 | 7.621 ± 0.2235 | 923.8 ± 2.491 | 2.421e+04 ± 71.4 | 2.421e+04 ± 71.53 | 2.597 ± 0.02404 |

Methods: AMD Ryzen 7 2700 Eight-Core Processor; 16GB DDR4;
Samsung SSD 990 EVO Plus 2TB;
Windows: Version 10.0.26200 (this is a Windows 11 Insider Preview build); 
.NET SDK: 10.0.301.
Sample size 3.

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

## Reference
* [pldb](http://pldb.info/concepts/java)

