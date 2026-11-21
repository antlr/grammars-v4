# XQuery 4.0 Grammar

ANTLR4 grammar for XQuery 4.0.

* Specification: https://qt4cg.org/pr/2796/xquery-40/xquery-40-autodiff.html
* Grammar Explorer: https://qt4cg.org/specifications/grammar-explorer/xquery40/index.html NB: Differs from Spec; Incorrect syntax.
* XQuery Update: https://www.w3.org/TR/xquery-update-10/#id-grammar

## Grammar Files

| File | Description |
|------|-------------|
| `XQuery4Lexer.g4` | Lexer grammar with lexer modes for direct element constructors |
| `XQuery4Parser.g4` | Parser grammar covering all XQuery 4.0 constructs |
| `CSharp/XQuery4LexerBase.cs` | C# lexer base class (`IsNCNameStart`, `PopModeIfNeeded`) |
| `CSharp/XQuery4ParserBase.cs` | C# parser base class (`IsFuncCall`) |
| `Java/XQuery4LexerBase.java` | Java equivalents |
| `Java/XQuery4ParserBase.java` | |
| `JavaScript/XQuery4LexerBase.js` | JavaScript equivalents |
| `JavaScript/XQuery4ParserBase.js` | |

## Entry Points

| Rule | Use |
|------|-----|
| `querylist` | Full XQuery file (one or more modules) |
| `module_` | Single XQuery module |
| `auxilary` | Semicolon-separated modules/expressions (used for testing) |

## Key XQuery 4.0 Features

- **Module structure**: `xquery version "4.0";`, library modules (`module namespace ...`), prolog declarations
- **FLWOR expressions**: `for`, `let`, `where`, `order by`, `group by`, `count`, `window`, `while`, `trace` clauses
- **For bindings**: Standard (`for $x in`), member (`for member $m in`), entry (`for key $k value $v in`)
- **Let destructuring**: sequence (`let $($x) :=`), array (`let $[$x] :=`), map (`let ${$x} :=`)
- **AllowingEmpty**: `for $x allowing empty in expr`
- **Window clauses**: `tumbling window`, `sliding window`
- **Switch / Typeswitch**: `switch($x) case ... default return`, `typeswitch($x) case ... default return`
- **Try/Catch/Finally**: `try { } catch * { } finally { }`
- **Validate**: `validate lax { }`, `validate strict { }`, `validate type T { }`
- **Extension expressions**: `(# pragma #) { expr }`
- **Direct constructors**: `<element attr="value">content</element>` (requires lexer modes)
- **Computed constructors**: `element`, `attribute`, `text`, `comment`, `document`, `namespace`, `processing-instruction`
- **String templates**: `` `Hello {$name}` ``
- **Record types**: `record(name as xs:string, age? as xs:integer)`, `record(*)`
- **Enumeration types**: `enum("red", "green", "blue")`
- **Named record type declarations**: `declare record Point = record(x as xs:double, y as xs:double)`
- **Type aliases**: `declare type MyType = xs:integer+`
- **Annotations**: `%private`, `%public`, `%rest:path("/api")`
- **XPath 4.0 inheritance**: all XPath 4.0 features (otherwise, record-put `:=+`, mapping arrow `=!>`, method arrow `=?>`, string templates, etc.)

## Lexer Modes

Direct element constructors use four lexer modes:

| Mode | Entered when | Exited when |
|------|-------------|-------------|
| `IN_ELEMENT_TAG` | `<name` in DEFAULT or element content | `>` or `/>` |
| `IN_ATTR_VALUE_QUOT` | `"` in element tag | closing `"` |
| `IN_ATTR_VALUE_APOS` | `'` in element tag | closing `'` |
| `IN_ELEMENT_CONTENT` | `>` closes open tag | `</name>` |
| `IN_CLOSE_TAG` | `</` in element content | `>` |

Embedded expressions (`{ expr }`) within element/attribute content push `DEFAULT_MODE`
onto the mode stack. The `}` token uses `PopModeIfNeeded()` (in the lexer base class)
to return to the previous mode.

## Building

```sh
cd xquery/xquery4
dotnet trash gen -t CSharp
cd Generated-CSharp
bash build.sh
bash test.sh
```
