# Java grammars

Here's what's in this folder (and why there are multiple Java grammars):

## java/java

Purpose: the "general-purpose" Java grammar most people should start with.
Notes: It's intended to cover Java 24 and it is maintained
to be more practical/performant than the spec-literal grammar.
Even ANTLR maintainers have suggested using this variant rather than
the strict Java 8 spec grammar for most work. 

## java/java8

Purpose: a spec-faithful transcription of the Java 8 grammar from the JLS.
Notes: This one mirrors the spec's structure very closely. You'll see _lf_/_lfno_
helper nonterminals that come straight from the JLS text. It's useful when you
want something that matches the book exactly, but it's heavier and typically
slower, and parts are considered dated. 

## java/java9

Purpose: a grammar variant adjusted for Java 9 features, most notably the module system (Jigsaw).
Notes: It exists so tools that care specifically about Java 9 syntax (e.g., module-info.java)
don't have to mix version conditionals into a single grammar file. 
File listing references show a separate java9 grammar set alongside java8. 

## java/java20
Purpose: It's a Java 20-targeted grammar, intended to support Java 20 syntax (like variadic
record patterns, sealed classes enhancements, pattern matching, etc.).

Use case: Allows parsing Java 20 constructs directly, without manually patching older grammars.

Evolution: Users may use it as a base for newer versions like Java 21 or beyond, potentially by
forking and adapting it.



## Why they're different

### Version targeting.
Java's syntax has evolved (lambdas in 8, modules in 9, etc.). Keeping version-specific grammars (java8, java9) lets you parse codebases pinned to a language level without ambiguity. 

### Spec-literal vs. pragmatic.
The java8 grammar hews tightly to the JLS (including those _lf_ / _lfno_ rules), which is great for research or spec comparison, while java/java trades some spec literalness for practicality and speed. Community guidance often points people to java/java for real projects. 

### Maintenance status/perf.
Community discussions note that the java8 (and older java9) grammars are comparatively older/slower and need love; the "java/java" grammar is typically the better maintained day-to-day option. 

## Quick guidance
Need a working parser for modern Java? Use java/java first. 

Need exact JLS-8 correspondence or you're doing spec-driven research? Use java/java8 (expect verbosity and slower performance). 

Parsing Java 9 modules specifically? Use java/java9 or merge its module rules into your base grammar. 
