# Java grammars

Here's what's in this folder and why there are multiple Java grammars:

## [java](./java)

Purpose: the "general-purpose" Java grammar most people should start with.
Notes: It's intended to cover Java 24, and it is maintained to be more practical/performant than the spec-literal grammar.
ANTLR maintainers suggest using this variant rather than the strict Java 8, 9, or 20 spec grammars for most work.

## [java8](./java8)

Purpose: a faithful transcription of the Java 8 grammar from the JLS.
Notes: This one mirrors the spec's structure very closely.
You'll see _lf_/_lfno_ helper nonterminals that come straight from the JLS text.
There are no refactorings from the spec EBNF, except for removal of the mutual
left recursion in the spec EBNF. The grammar is useful when you
want something that matches the book exactly, but it's
more verbose (e.g., more rules that have a single symbol on the right-hand
side, operator-precedence rules for expressions) and typically slower.

## [java9](./java9)

Purpose: a grammar variant derived from JLS 9, which include most notably syntax for module. Notes: It exists for developers that care specifically about Java 9 syntax (e.g., module-info.java) don't have to mix version conditionals into a single grammar file. 

## [java20](./java20)
Purpose: It's a Java 20-targeted grammar, intended to support Java 20 syntax (like variadic
record patterns, sealed classes enhancements, pattern matching, etc.).

Use case: Allows parsing Java 20 constructs directly, without manually patching older grammars.

## Why they're different

### Version targeting.
Java's syntax has evolved (lambdas in 8, modules in 9, etc.). Keeping version-specific grammars (java8, java9, java20) lets you parse codebases pinned to a language level. 

### Spec-literal vs. pragmatic.
The java8, java9, and java20 grammars hew tightly to the JLS, which is good for research or spec comparison, while java/java trades some spec literalness for practicality and speed.

## Quick guidance
Need a working parser for modern Java? Use java/java first. 

Need exact JLS correspondence? Use java8. 9, or 20, but expect much slower performance. 

