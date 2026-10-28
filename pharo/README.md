# Pharo Grammar

An ANTLR4 grammar for a practical subset of [Pharo](https://pharo.org/), a modern,
open-source Smalltalk-80 dialect.

Pharo's dynamic, extensible syntax (user-defined message selectors, no fixed
keyword set) and its usual delivery as compiled method/class metadata rather than
flat source text mean a single context-free grammar cannot capture every nuance of
a live Pharo image. This grammar instead covers the classic fileout-style surface
syntax that Pharo source commonly appears as:

* class definitions, e.g. `Object subclass: #Foo instanceVariableNames: '...'
  classVariableNames: '...' package: '...'`
* method definitions using the `ClassName >> selector` / `ClassName class >>
  selector` form, including unary, binary, and keyword method patterns
* pragmas (e.g. `<primitive: 60>`, `<category: 'accessing'>`)
* the full Smalltalk message-send language: unary, binary, and keyword message
  sends with the correct relative precedence, cascades (`;`), assignment
  (`:=`), blocks (`[:x | ...]`), and non-local return (`^`)
* literals: integers (including radix, e.g. `16rFF`), floats, strings,
  characters (`$a`), symbols (`#foo`, `#at:put:`, `#+`, `#'quoted symbol'`),
  literal arrays (`#(1 2 3)`), byte arrays (`#[1 2 3]`), and dynamic arrays
  (`{1 + 2. 3. x}`)

Reference: <https://pharo.org/>
