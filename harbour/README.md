# Harbour Grammar

An ANTLR4 grammar for a practical subset of [Harbour](https://harbour.github.io/),
a modern, cross-platform, Clipper/xBase-compatible compiler.

Harbour's full language includes many command-style extensions, a preprocessor
with user-definable macros/translates, and case-insensitive keywords, which a
single context-free ANTLR4 grammar cannot fully capture. This grammar instead
covers the common, fixed core of everyday Harbour source, with keywords matched
in the conventional uppercase style:

* `FUNCTION` / `PROCEDURE` declarations with parameter lists
* `CLASS` / `ENDCLASS` declarations (`DATA`/`VAR` members, `METHOD` declarations,
  including inline methods and constructors) and out-of-class `METHOD ... CLASS`
  implementations
* variable declarations: `LOCAL`, `STATIC`, `PUBLIC`, `PRIVATE`, `FIELD`, with
  `:=` or `INIT` initializers and optional `AS` type hints
* control flow: `IF`/`ELSEIF`/`ELSE`/`ENDIF`, `DO WHILE`/`ENDDO`,
  `FOR`/`TO`/`STEP`/`NEXT`, `DO CASE`/`CASE`/`OTHERWISE`/`ENDCASE`, and
  `SWITCH`/`CASE`/`OTHERWISE`/`END`
* `RETURN`, `EXIT`, `LOOP`, and the `?` print command
* the expression language: assignment, arithmetic/relational/logical operators
  (including the dotted forms `.AND.`, `.OR.`, `.NOT.`), object messages
  (`oObj:Method()`), the self-reference operator (`::Field`), the macro
  operator (`&cVar`), array indexing/literals (`{1, 2, 3}`), and code blocks
  (`{|x| x * x}`)

Reference: <https://harbour.github.io/>
