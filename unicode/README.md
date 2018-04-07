# Unicode-9.0.0

## Highlights

| FILE              | CONTENTS                                            |
| ----------------- | --------------------------------------------------- |
| classify16.g4     | ANTLR4 import grammar for codepoint classification  |
| classify21.g4     | 21 bit import grammar for codepoint classification  |
| README.grammar.md | This file                                           |

Unicode.org maintains authoritative/normative sources for all unicode data.
These data are stored in rigorously maintained files with rules for changes.
The latest versions of these files are understood to be the final word.

Unicode is a universal meta-alphabet containing
all active world language alphabets and some constructed alphabets.
A unicode character is assigned a codepoint which is an unique ordinal.
Codepoints are constrained to a range of values between 0 and 1114111
(between 0 and 0x10FFFF).

The English alphabet is directly copied into ASCII 0-127 (0-0x7f).
Other alphabets are given in one or more codepoint blocks.
As unicode evolves, codepoint blocks are added to contain characters
that had been overlooked in previous versions of unicode.

21 bits are required to represent the full unicode meta-alphabet.
Unicode supporting software is often limited to 16 bit codepoints.
This becomes a dilemma for lexers/parsers since codepoints for
some significant languages include code blocks needing more than 16 bits.

In addition to maintaining lists of character codepoints,
unicode.org also maintains codepoint classifications.
There are currently 38 classifications in unicode 9.0.0 of which
31 are used directly on codepoints. 8 are used to aggregate classifications.
For instance Ll(Letter lowercase) and Lu(Letter uppercase) are combined into
L(Letter) in lexers, but do not appear as direct classifications.
UnicodeData.txt column 3 and the gc list in PropertyValueAliases.txt
contain and use these classifications.

```
gc ; C  ; Other                  # Cc | Cf | Cn | Co | Cs
gc ; Cc ; Control                ; cntrl
gc ; Cf ; Format
gc ; Cn ; Unassigned
gc ; Co ; Private_Use
gc ; Cs ; Surrogate
gc ; L  ; Letter                 # Ll | Lm | Lo | Lt | Lu
gc ; LC ; Cased_Letter           # Ll | Lt | Lu
gc ; Ll ; Lowercase_Letter
gc ; Lm ; Modifier_Letter
gc ; Lo ; Other_Letter
gc ; Lt ; Titlecase_Letter
gc ; Lu ; Uppercase_Letter
gc ; M  ; Mark                   ; Combining_Mark  # Mc | Me | Mn
gc ; Mc ; Spacing_Mark
gc ; Me ; Enclosing_Mark
gc ; Mn ; Nonspacing_Mark
gc ; N  ; Number                 # Nd | Nl | No
gc ; Nd ; Decimal_Number         ; digit
gc ; Nl ; Letter_Number
gc ; No ; Other_Number
gc ; P  ; Punctuation            ; punct  # Pc | Pd | Pe | Pf | Pi | Po | Ps
gc ; Pc ; Connector_Punctuation
gc ; Pd ; Dash_Punctuation
gc ; Pe ; Close_Punctuation
gc ; Pf ; Final_Punctuation
gc ; Pi ; Initial_Punctuation
gc ; Po ; Other_Punctuation
gc ; Ps ; Open_Punctuation
gc ; S  ; Symbol                 # Sc | Sk | Sm | So
gc ; Sc ; Currency_Symbol
gc ; Sk ; Modifier_Symbol
gc ; Sm ; Math_Symbol
gc ; So ; Other_Symbol
gc ; Z  ; Separator              # Zl | Zp | Zs
gc ; Zl ; Line_Separator
gc ; Zp ; Paragraph_Separator
gc ; Zs ; Space_Separator
```

The two grammars offered here are produced by a
[means for production](https://github.com/jlettvin/authoritative/tree/master/Unicode).

To these classifications, __ has been added as an ERROR classification
to indicated a codepoint which is either not classified or
absent from the normative list.

These ANTLR4 grammars are expressions of unicode.org normative data
and fill different requirements.
Classify16 is necessary but insufficient.
Classify21 is necessary and sufficient but
fails due to unicode width limitations in the java used in ANTLR4.

* classify16.g4 works with the existing 16 bit java ANTLR4
* classify21.g4 works with a theoretically complete 21 bit java ANTLR4
