/*
 * Scala 3 Lexer Grammar
 * Based on https://docs.scala-lang.org/scala3/reference/syntax.html
 *
 * Supports both brace-delimited and indentation-sensitive Scala 3.
 * INDENT/DEDENT tokens are injected by Scala3LexerBase; NEWLINE tokens
 * are emitted by the grammar (not skipped) so the base class can count
 * leading whitespace.  WS is sent to the hidden channel for the same reason.
 */

lexer grammar Scala3Lexer;

options { superClass=Scala3LexerBase; }

// Synthetic tokens injected by Scala3LexerBase (no lexer rule matches them).
tokens { INDENT, DEDENT }

// -----------------------------------------------------------------------
// Keywords — must come before identifier rules
// -----------------------------------------------------------------------

ABSTRACT    : 'abstract';
AS          : 'as';
CASE        : 'case';
CATCH       : 'catch';
CLASS       : 'class';
DEF         : 'def';
DERIVES     : 'derives';
DO          : 'do';
ELSE        : 'else';
END         : 'end';
ENUM        : 'enum';
EXPORT      : 'export';
EXTENDS     : 'extends';
EXTENSION   : 'extension';
FINAL       : 'final';
FINALLY     : 'finally';
FOR         : 'for';
GIVEN       : 'given';
IF          : 'if';
IMPLICIT    : 'implicit';
IMPORT      : 'import';
INFIX       : 'infix';
INLINE      : 'inline';
LAZY        : 'lazy';
MATCH       : 'match';
NEW         : 'new';
OBJECT      : 'object';
OPEN        : 'open';
OPAQUE      : 'opaque';
OVERRIDE    : 'override';
PACKAGE     : 'package';
PRIVATE     : 'private';
PROTECTED   : 'protected';
RETURN      : 'return';
SEALED      : 'sealed';
SUPER       : 'super';
THEN        : 'then';
THIS        : 'this';
THROW       : 'throw';
TRAIT       : 'trait';
TRANSPARENT : 'transparent';
TRY         : 'try';
TYPE        : 'type';
USING       : 'using';
VAL         : 'val';
VAR         : 'var';
WHILE       : 'while';
WITH        : 'with';
YIELD       : 'yield';

// -----------------------------------------------------------------------
// Boolean / null literals — before Id so they win
// -----------------------------------------------------------------------

BooleanLiteral : 'true' | 'false';
NullLiteral    : 'null';

// -----------------------------------------------------------------------
// Integer literals
// -----------------------------------------------------------------------

IntegerLiteral
    : (DecimalNumeral | HexNumeral | BinaryNumeral) [Ll]?
    ;

// -----------------------------------------------------------------------
// Floating-point literals
// -----------------------------------------------------------------------

FloatingPointLiteral
    : DecimalNumeral '.' [0-9] ([0-9_]* [0-9])? ExponentPart? FloatType?
    | '.' [0-9] ([0-9_]* [0-9])? ExponentPart? FloatType?
    | DecimalNumeral ExponentPart FloatType?
    | DecimalNumeral FloatType
    ;

// -----------------------------------------------------------------------
// Character literal
// -----------------------------------------------------------------------

CharacterLiteral
    : '\'' (PrintableChar | CharEscapeSeq) '\''
    ;

// -----------------------------------------------------------------------
// String literals
// -----------------------------------------------------------------------

StringLiteral
    : '"' StringElement* '"'
    | '"""' MultiLineChars '"""'
    ;

// -----------------------------------------------------------------------
// Interpolated string literals
// -----------------------------------------------------------------------

InterpolatedStringLiteral
    : AlphaId '"'  InterpStringPart* '"'
    | AlphaId '"""' InterpMultiPart* '"""'
    ;

// -----------------------------------------------------------------------
// Symbol literal (deprecated in Scala 3 but still parseable)
// NOTE: must come AFTER CharacterLiteral to avoid stealing single-char '\''Id
// -----------------------------------------------------------------------

SymbolLiteral
    : '\'' PlainId
    ;

// -----------------------------------------------------------------------
// Quote id for quoted expressions — '\'' followed by an alphanumeric id
// -----------------------------------------------------------------------

QuoteId
    : '\'' AlphaId
    ;

// -----------------------------------------------------------------------
// Standalone single-quote — for '{ block } and '[ type ] quoted expressions.
// Defined AFTER CharacterLiteral/SymbolLiteral/QuoteId so longest match wins:
//   'a'   -> CharacterLiteral (3 chars)
//   'foo  -> SymbolLiteral or QuoteId (>1 char)
//   '{    -> QUOTE (1 char, since CharacterLiteral/Symbol/QuoteId all fail)
// -----------------------------------------------------------------------

QUOTE
    : '\''
    ;

// -----------------------------------------------------------------------
// Backtick-quoted identifier
// -----------------------------------------------------------------------

BacktickId
    : '`' (~[`\r\n] | CharEscapeSeq | UnicodeEscape)+ '`'
    ;

// -----------------------------------------------------------------------
// Grouped delimiters
// -----------------------------------------------------------------------

LPAREN   : '(';
RPAREN   : ')';
LBRACKET : '[';
RBRACKET : ']';
LBRACE   : '{';
RBRACE   : '}';
COMMA    : ',';
SEMI     : ';';
DOT      : '.';
AT       : '@';
HASH     : '#';
USCORE   : '_';

// -----------------------------------------------------------------------
// Operator tokens — LONGEST match first within the same prefix cluster.
//
// The Scala 3 type-lambda arrow is the three characters: = > >
// Written as an ANTLR string literal: '->>'  — NO.
// The character '=' is U+003D, '>' is U+003E.
// Written as an ANTLR string literal with those three chars: '=>>'
// (single-quote, equals-sign, greater-than, greater-than, single-quote)
//
// Context function arrow: ?=>  (chars: ? = >)
// -----------------------------------------------------------------------

// Multi-char operator tokens — longest first within same prefix cluster.
//
//   ?=>   context function arrow   (chars: ?, =, >)
//   =>>   type lambda arrow        (chars: =, >, >)   written as '=>>'
//   =>    function arrow           (chars: =, >)
//   <-    generator arrow          (chars: <, -)
//   <:    upper type bound         (chars: <, :)
//   >:    lower type bound         (chars: >, :)
//
CTXARROW  : '?=>';
TLARROW   : '=>>';       // =  >  >   (equals, greater, greater)
ARROW     : '=>';
LARROW    : '<-';
SUBTYPE   : '<:';
SUPERTYPE : '>:';

// Single-char tokens that are also opchars but need names in the parser
ASSIGN    : '=';
QUESTION  : '?';
COLON     : ':';

// -----------------------------------------------------------------------
// General operator — catches all remaining opchar sequences
// -----------------------------------------------------------------------

Op
    : Opchar+
    ;

// -----------------------------------------------------------------------
// Identifiers (after all keywords so keywords win)
// -----------------------------------------------------------------------

Id
    : Upper Idrest
    ;

Varid
    : Lower Idrest
    ;

// -----------------------------------------------------------------------
// Whitespace and comments (skip)
// -----------------------------------------------------------------------

// Emitted as a real token so Scala3LexerBase can inject INDENT/DEDENT/SEMI.
// The base class hides every NEWLINE from the parser and replaces it with
// the appropriate synthetic token(s).
NEWLINE
    : '\r'? '\n'
    ;

// Sent to the hidden channel so Scala3LexerBase can read leading whitespace
// (needed for indentation measurement) while the parser ignores all WS.
WS
    : [ \t\u000C]+ -> channel(HIDDEN)
    ;

BlockComment
    : '/*' (BlockComment | .)*? '*/' -> skip
    ;

LineComment
    : '//' ~[\r\n]* -> skip
    ;

// -----------------------------------------------------------------------
// Fragments
// -----------------------------------------------------------------------

fragment AlphaId
    : Upper Idrest
    | Lower Idrest
    ;

fragment PlainId
    : AlphaId
    | Op
    ;

// idrest = {letter | digit} ['_' op]
// _ is in Lower so it's included in {letter|digit} via Letter
fragment Idrest
    : (Letter | [0-9])* ('_' Opchar+)?
    ;

// Upper includes $ and _ so that _foo and $Foo are valid identifiers
fragment Upper
    : [A-Z$_]
    | UnicodeLU
    | UnicodeLT
    ;

fragment Lower
    : [a-z]
    | UnicodeLL
    ;

fragment Letter
    : Upper
    | Lower
    | UnicodeLM
    | UnicodeLO
    ;

// All characters that can appear in an operator
fragment Opchar
    : [!#%&*+\-/<=>?@\\^|~:]
    ;

fragment DecimalNumeral
    : '0'
    | [1-9] ([0-9_]* [0-9])?
    ;

fragment HexNumeral
    : '0' [xX] [0-9A-Fa-f] ([0-9A-Fa-f_]* [0-9A-Fa-f])?
    ;

fragment BinaryNumeral
    : '0' [bB] [01] ([01_]* [01])?
    ;

fragment ExponentPart
    : [Ee] [+\-]? [0-9] ([0-9_]* [0-9])?
    ;

fragment FloatType
    : [FfDd]
    ;

fragment PrintableChar
    : [\u0020-\u007E]
    ;

fragment CharEscapeSeq
    : '\\' [btnfr"'\\]
    ;

fragment UnicodeEscape
    : '\\' 'u'+ [0-9A-Fa-f] [0-9A-Fa-f] [0-9A-Fa-f] [0-9A-Fa-f]
    ;

fragment StringElement
    : ~["\\\r\n]
    | CharEscapeSeq
    | UnicodeEscape
    ;

// Triple-quoted string body: any content that doesn't end with """
fragment MultiLineChars
    : (~["] | '"' ~["] | '""' ~["])*
    ;

fragment InterpStringPart
    : ~["$\\\r\n]+
    | '\\' .
    | '$' [a-zA-Z_$] [a-zA-Z0-9_$]*
    | '$' '{' InterpExprContent* '}'
    | '$$'
    ;

fragment InterpMultiPart
    : ~["$\\]+
    | '\\' .
    | '"' ~["]
    | '""' ~["]
    | '$' [a-zA-Z_$] [a-zA-Z0-9_$]*
    | '$' '{' InterpMultiExprContent* '}'
    | '$$'
    ;

// Content of a ${ } block in an interpolated string.
// Handles one level of nested { }, double-quoted string literals, and escape
// sequences — enough to cover cases like ${map(m => s"${m.x}").mkString("\n")}.
fragment InterpExprContent
    : ~[{}"\\\r\n]+
    | '\\' .
    | '"' (~["\\\r\n] | '\\' .)* '"'
    | '{' InterpExprContent0* '}'
    ;

// One additional level of { } nesting inside InterpExprContent.
fragment InterpExprContent0
    : ~[{}"\\\r\n]+
    | '\\' .
    | '"' (~["\\\r\n] | '\\' .)* '"'
    | '{' ~[{}]* '}'
    ;

// Like InterpExprContent but allows actual newlines — for use inside """..."""
// where the ${ } expression may span multiple source lines.
fragment InterpMultiExprContent
    : ~[{}"\\]+
    | '\\' .
    | '"' (~["\\] | '\\' .)* '"'
    | '{' InterpMultiExprContent0* '}'
    ;

fragment InterpMultiExprContent0
    : ~[{}"\\]+
    | '\\' .
    | '"' (~["\\] | '\\' .)* '"'
    | '{' ~[{}]* '}'
    ;

// -----------------------------------------------------------------------
// Unicode categories (abbreviated BMP coverage)
// -----------------------------------------------------------------------

fragment UnicodeLU
    : '\u00C0'..'\u00D6' | '\u00D8'..'\u00DE'
    | '\u0100'..'\u0136' | '\u0139'..'\u0147' | '\u014A'..'\u0178'
    | '\u0179'..'\u017D' | '\u0181'..'\u0182' | '\u0184'..'\u0186'
    | '\u0187'..'\u0189' | '\u018A'..'\u018B' | '\u018E'..'\u0191'
    | '\u0193'..'\u0194' | '\u0196'..'\u0198' | '\u019C'..'\u019D'
    | '\u019F'..'\u01A0' | '\u01A2'..'\u01A6' | '\u01A7'..'\u01A9'
    | '\u01AC'..'\u01AE' | '\u01AF'..'\u01B1' | '\u01B2'..'\u01B3'
    | '\u01B5'..'\u01B7' | '\u01B8'..'\u01BC' | '\u01C4'..'\u01CD'
    | '\u01CF'..'\u01DB' | '\u01DE'..'\u01EE' | '\u01F1'..'\u01F4'
    | '\u01F6'..'\u01F8' | '\u01FA'..'\u0232' | '\u0370'..'\u0372'
    | '\u0386' | '\u0388'..'\u038A' | '\u038C' | '\u038E'..'\u03A1'
    | '\u03A3'..'\u03AB' | '\u03D8'..'\u03EE' | '\u03F4' | '\u03F7'
    | '\u03F9'..'\u03FA' | '\u03FD'..'\u042F' | '\u0460'..'\u0480'
    | '\u048A'..'\u04C0' | '\u04C1'..'\u04CD' | '\u04D0'..'\u052E'
    | '\u0531'..'\u0556' | '\u10A0'..'\u10C5'
    | '\u1E00'..'\u1E94' | '\u1E9E'..'\u1EFE'
    | '\u1F08'..'\u1F0F' | '\u1F18'..'\u1F1D' | '\u1F28'..'\u1F2F'
    | '\u1F38'..'\u1F3F' | '\u1F48'..'\u1F4D' | '\u1F59'..'\u1F5F'
    | '\u1F68'..'\u1F6F' | '\u1FB8'..'\u1FBB' | '\u1FC8'..'\u1FCB'
    | '\u1FD8'..'\u1FDB' | '\u1FE8'..'\u1FEC' | '\u1FF8'..'\u1FFB'
    | '\u2102' | '\u2107' | '\u210B'..'\u210D' | '\u2110'..'\u2112'
    | '\u2115' | '\u2119'..'\u211D' | '\u2124' | '\u2126' | '\u2128'
    | '\u212A'..'\u212D' | '\u2130'..'\u2133' | '\u213E'..'\u213F'
    | '\u2145' | '\u2183'
    | '\u2C00'..'\u2C2E' | '\u2C60'..'\u2C62' | '\u2C67'..'\u2C6D'
    | '\u2C72' | '\u2C75' | '\u2C7E'..'\u2C80' | '\u2C82'..'\u2CE2'
    | '\u2CEB'..'\u2CED'
    | '\uA722'..'\uA72E' | '\uA732'..'\uA76E' | '\uA779'..'\uA77D'
    | '\uA77E'..'\uA786' | '\uA78B'..'\uA78D' | '\uA790'..'\uA792'
    | '\uA796'..'\uA7AA' | '\uFF21'..'\uFF3A'
    ;

fragment UnicodeLL
    : '\u00B5' | '\u00DF'..'\u00F6' | '\u00F8'..'\u00FF'
    | '\u0101'..'\u0137' | '\u0138'..'\u0148' | '\u0149'..'\u0177'
    | '\u017A'..'\u017E' | '\u017F'..'\u0180' | '\u0183'..'\u0185'
    | '\u0188'..'\u018C' | '\u018D'..'\u0192' | '\u0195'..'\u0199'
    | '\u019A'..'\u019B' | '\u019E'..'\u01A1' | '\u01A3'..'\u01A5'
    | '\u01A8'..'\u01AA' | '\u01AB'..'\u01AD' | '\u01B0'..'\u01B4'
    | '\u01B6'..'\u01B9' | '\u01BA'..'\u01BD' | '\u01BF'
    | '\u01C6' | '\u01C9' | '\u01CC' | '\u01CE'..'\u01DC'
    | '\u01DD'..'\u01EF' | '\u01F0'..'\u01F3' | '\u01F5'..'\u01F9'
    | '\u01FB'..'\u0233' | '\u023C'..'\u023F' | '\u0247'..'\u024F'
    | '\u0250'..'\u0293' | '\u0295'..'\u02AF'
    | '\u0371'..'\u0373' | '\u0377'..'\u037B' | '\u037C'..'\u037D'
    | '\u0390' | '\u03AC'..'\u03CE' | '\u03D0'..'\u03D1'
    | '\u03D5'..'\u03D7' | '\u03D9'..'\u03EF' | '\u03F0'..'\u03F3'
    | '\u03F5' | '\u03FB'..'\u0430' | '\u0431'..'\u045F'
    | '\u0461'..'\u0481' | '\u048B'..'\u04BF' | '\u04C2'..'\u04CE'
    | '\u04CF'..'\u052F' | '\u0561'..'\u0587'
    | '\u1D00'..'\u1D2B' | '\u1D6B'..'\u1D77' | '\u1D79'..'\u1D9A'
    | '\u1E01'..'\u1E95' | '\u1E9F'..'\u1EFF'
    | '\u1F00'..'\u1F07' | '\u1F10'..'\u1F15' | '\u1F20'..'\u1F27'
    | '\u1F30'..'\u1F37' | '\u1F40'..'\u1F45' | '\u1F50'..'\u1F57'
    | '\u1F60'..'\u1F67' | '\u1F70'..'\u1F7D' | '\u1F80'..'\u1F87'
    | '\u1F90'..'\u1F97' | '\u1FA0'..'\u1FA7' | '\u1FB0'..'\u1FB4'
    | '\u1FB6'..'\u1FB7' | '\u1FBE' | '\u1FC2'..'\u1FC4'
    | '\u1FC6'..'\u1FC7' | '\u1FD0'..'\u1FD3' | '\u1FD6'..'\u1FD7'
    | '\u1FE0'..'\u1FE7' | '\u1FF2'..'\u1FF4' | '\u1FF6'..'\u1FF7'
    | '\u210A' | '\u210E'..'\u210F' | '\u2113' | '\u212F' | '\u2134'
    | '\u2139' | '\u213C'..'\u213D' | '\u2146'..'\u2149' | '\u214E'
    | '\u2184'
    | '\u2C30'..'\u2C5E' | '\u2C61' | '\u2C65'..'\u2C66'
    | '\u2C68'..'\u2C6C' | '\u2C71' | '\u2C73'..'\u2C74'
    | '\u2C76'..'\u2C7B' | '\u2C81'..'\u2CE3' | '\u2CEE'..'\u2CF3'
    | '\u2D00'..'\u2D25'
    | '\uA641'..'\uA66D' | '\uA681'..'\uA69B' | '\uA723'..'\uA72F'
    | '\uA730'..'\uA731' | '\uA733'..'\uA771' | '\uA772'..'\uA778'
    | '\uA77A'..'\uA77C' | '\uA77F'..'\uA787' | '\uA78C'..'\uA78E'
    | '\uA791'..'\uA793' | '\uA797'..'\uA7A9' | '\uA7FA'
    | '\uFB00'..'\uFB06' | '\uFB13'..'\uFB17' | '\uFF41'..'\uFF5A'
    ;

fragment UnicodeLT
    : '\u01C5' | '\u01C8' | '\u01CB' | '\u01F2'
    | '\u1F88'..'\u1F8F' | '\u1F98'..'\u1F9F' | '\u1FA8'..'\u1FAF'
    | '\u1FBC' | '\u1FCC' | '\u1FFC'
    ;

fragment UnicodeLM
    : '\u02B0'..'\u02C1' | '\u02C6'..'\u02D1' | '\u02E0'..'\u02E4'
    | '\u02EC' | '\u02EE' | '\u0374' | '\u037A' | '\u0559' | '\u0640'
    | '\u06E5'..'\u06E6' | '\u07F4'..'\u07F5' | '\u07FA'
    | '\u2071' | '\u207F' | '\u2090'..'\u209C'
    | '\u2C7C'..'\u2C7D' | '\u3005' | '\u3031'..'\u3035'
    | '\u309D'..'\u309E' | '\u30FC'..'\u30FE'
    | '\uA9CF' | '\uFF70' | '\uFF9E'..'\uFF9F'
    ;

fragment UnicodeLO
    : '\u00AA' | '\u00B5' | '\u00BA'
    | '\u01BB' | '\u01C0'..'\u01C3' | '\u0294'
    | '\u05D0'..'\u05EA' | '\u05F0'..'\u05F2'
    | '\u0620'..'\u063F' | '\u0641'..'\u064A' | '\u066E'..'\u066F'
    | '\u0671'..'\u06D3' | '\u06D5' | '\u06EE'..'\u06EF'
    | '\u06FA'..'\u06FC' | '\u06FF' | '\u0710' | '\u0712'..'\u072F'
    | '\u074D'..'\u07A5' | '\u07B1' | '\u07CA'..'\u07EA'
    | '\u0800'..'\u0815' | '\u0840'..'\u0858' | '\u08A0'..'\u08B2'
    | '\u0904'..'\u0939' | '\u093D' | '\u0950' | '\u0958'..'\u0961'
    | '\u0972'..'\u0980' | '\u0985'..'\u098C' | '\u098F'..'\u0990'
    | '\u0993'..'\u09A8' | '\u09AA'..'\u09B0' | '\u09B2'
    | '\u09B6'..'\u09B9' | '\u09BD' | '\u09CE'
    | '\u09DC'..'\u09DD' | '\u09DF'..'\u09E1' | '\u09F0'..'\u09F1'
    | '\u0A05'..'\u0A0A' | '\u0A0F'..'\u0A10' | '\u0A13'..'\u0A28'
    | '\u0A2A'..'\u0A30' | '\u0A32'..'\u0A33' | '\u0A35'..'\u0A36'
    | '\u0A38'..'\u0A39' | '\u0A59'..'\u0A5C' | '\u0A5E'
    | '\u0A72'..'\u0A74' | '\u0A85'..'\u0A8D' | '\u0A8F'..'\u0A91'
    | '\u0A93'..'\u0AA8' | '\u0AAA'..'\u0AB0' | '\u0AB2'..'\u0AB3'
    | '\u0AB5'..'\u0AB9' | '\u0ABD' | '\u0AD0' | '\u0AE0'..'\u0AE1'
    | '\u0B05'..'\u0B0C' | '\u0B0F'..'\u0B10' | '\u0B13'..'\u0B28'
    | '\u0B2A'..'\u0B30' | '\u0B32'..'\u0B33' | '\u0B35'..'\u0B39'
    | '\u0B3D' | '\u0B5C'..'\u0B5D' | '\u0B5F'..'\u0B61' | '\u0B71'
    | '\u0B83' | '\u0B85'..'\u0B8A' | '\u0B8E'..'\u0B90'
    | '\u0B92'..'\u0B95' | '\u0B99'..'\u0B9A' | '\u0B9C'
    | '\u0B9E'..'\u0B9F' | '\u0BA3'..'\u0BA4' | '\u0BA8'..'\u0BAA'
    | '\u0BAE'..'\u0BB9' | '\u0BD0'
    | '\u0C05'..'\u0C0C' | '\u0C0E'..'\u0C10' | '\u0C12'..'\u0C28'
    | '\u0C2A'..'\u0C39' | '\u0C3D' | '\u0C58'..'\u0C60'
    | '\u0C85'..'\u0C8C' | '\u0C8E'..'\u0C90' | '\u0C92'..'\u0CA8'
    | '\u0CAA'..'\u0CB3' | '\u0CB5'..'\u0CB9' | '\u0CBD' | '\u0CDE'
    | '\u0CE0'..'\u0CE1' | '\u0CF1'..'\u0CF2'
    | '\u0D05'..'\u0D0C' | '\u0D0E'..'\u0D10' | '\u0D12'..'\u0D3A'
    | '\u0D3D' | '\u0D4E' | '\u0D60'..'\u0D61' | '\u0D7A'..'\u0D7F'
    | '\u0D85'..'\u0D96' | '\u0D9A'..'\u0DB1' | '\u0DB3'..'\u0DBB'
    | '\u0DBD' | '\u0DC0'..'\u0DC6'
    | '\u0E01'..'\u0E30' | '\u0E32'..'\u0E33' | '\u0E40'..'\u0E45'
    | '\u0E81'..'\u0E82' | '\u0E84' | '\u0E87'..'\u0E88' | '\u0E8A'
    | '\u0E8D' | '\u0E94'..'\u0E97' | '\u0E99'..'\u0E9F'
    | '\u0EA1'..'\u0EA3' | '\u0EA5' | '\u0EA7' | '\u0EAA'..'\u0EAB'
    | '\u0EAD'..'\u0EB0' | '\u0EB2'..'\u0EB3' | '\u0EBD'
    | '\u0EC0'..'\u0EC4' | '\u0EDC'..'\u0EDF' | '\u0F00'
    | '\u0F40'..'\u0F47' | '\u0F49'..'\u0F6C' | '\u0F88'..'\u0F8C'
    | '\u1000'..'\u102A' | '\u103F' | '\u1050'..'\u1055'
    | '\u105A'..'\u105D' | '\u1061' | '\u1065'..'\u1066'
    | '\u106E'..'\u1070' | '\u1075'..'\u1081' | '\u108E'
    | '\u10D0'..'\u10FA' | '\u10FD'..'\u1248' | '\u124A'..'\u124D'
    | '\u1250'..'\u1256' | '\u1258' | '\u125A'..'\u125D'
    | '\u1260'..'\u1288' | '\u128A'..'\u128D' | '\u1290'..'\u12B0'
    | '\u12B2'..'\u12B5' | '\u12B8'..'\u12BE' | '\u12C0'
    | '\u12C2'..'\u12C5' | '\u12C8'..'\u12D6' | '\u12D8'..'\u1310'
    | '\u1312'..'\u1315' | '\u1318'..'\u135A' | '\u1380'..'\u138F'
    | '\u13A0'..'\u13F4' | '\u1401'..'\u166C' | '\u166F'..'\u167F'
    | '\u1681'..'\u169A' | '\u16A0'..'\u16EA' | '\u1700'..'\u170C'
    | '\u170E'..'\u1711' | '\u1720'..'\u1731' | '\u1740'..'\u1751'
    | '\u1760'..'\u176C' | '\u176E'..'\u1770' | '\u1780'..'\u17B3'
    | '\u17DC' | '\u1820'..'\u1842' | '\u1844'..'\u1877'
    | '\u1880'..'\u18A8' | '\u18AA' | '\u18B0'..'\u18F5'
    | '\u1900'..'\u191E' | '\u1950'..'\u196D' | '\u1970'..'\u1974'
    | '\u1980'..'\u19AB' | '\u19C1'..'\u19C7'
    | '\u1A00'..'\u1A16' | '\u1A20'..'\u1A54'
    | '\u1B05'..'\u1B33' | '\u1B45'..'\u1B4B' | '\u1B83'..'\u1BA0'
    | '\u1BAE'..'\u1BAF' | '\u1BBA'..'\u1BE5' | '\u1C00'..'\u1C23'
    | '\u1C4D'..'\u1C4F' | '\u1C5A'..'\u1C77'
    | '\u2135'..'\u2138' | '\u2D30'..'\u2D67' | '\u2D80'..'\u2D96'
    | '\u2DA0'..'\u2DA6' | '\u2DA8'..'\u2DAE' | '\u2DB0'..'\u2DB6'
    | '\u2DB8'..'\u2DBE' | '\u2DC0'..'\u2DC6' | '\u2DC8'..'\u2DCE'
    | '\u2DD0'..'\u2DD6' | '\u2DD8'..'\u2DDE'
    | '\u3006' | '\u303C' | '\u3041'..'\u3096' | '\u309F'
    | '\u30A1'..'\u30FA' | '\u30FF' | '\u3105'..'\u312D'
    | '\u3131'..'\u318E' | '\u31A0'..'\u31BA' | '\u31F0'..'\u31FF'
    | '\u3400'..'\u4DB5' | '\u4E00'..'\u9FCC'
    | '\uA000'..'\uA014' | '\uA016'..'\uA48C' | '\uA4D0'..'\uA4F7'
    | '\uA500'..'\uA60B' | '\uA610'..'\uA61F' | '\uA62A'..'\uA62B'
    | '\uA66E' | '\uA6A0'..'\uA6E5' | '\uA7F7'
    | '\uA7FB'..'\uA801' | '\uA803'..'\uA805' | '\uA807'..'\uA80A'
    | '\uA80C'..'\uA822' | '\uA840'..'\uA873' | '\uA882'..'\uA8B3'
    | '\uA8F2'..'\uA8F7' | '\uA8FB' | '\uA90A'..'\uA925'
    | '\uA930'..'\uA946' | '\uA960'..'\uA97C' | '\uA984'..'\uA9B2'
    | '\uA9E0'..'\uA9E4' | '\uA9E7'..'\uA9EF' | '\uA9FA'..'\uA9FE'
    | '\uAA00'..'\uAA28' | '\uAA40'..'\uAA42' | '\uAA44'..'\uAA4B'
    | '\uAA60'..'\uAA6F' | '\uAA71'..'\uAA76' | '\uAA7A'
    | '\uAA7E'..'\uAAB1' | '\uAAB5'..'\uAAB6' | '\uAAB9'..'\uAABD'
    | '\uAAC0' | '\uAAC2' | '\uAADB'..'\uAADC' | '\uAAE0'..'\uAAEA'
    | '\uAAF2' | '\uAB01'..'\uAB06' | '\uAB09'..'\uAB0E'
    | '\uAB11'..'\uAB16' | '\uAB20'..'\uAB26' | '\uAB28'..'\uAB2E'
    | '\uABC0'..'\uABE2' | '\uAC00'..'\uD7A3' | '\uD7B0'..'\uD7C6'
    | '\uD7CB'..'\uD7FB' | '\uF900'..'\uFA6D' | '\uFA70'..'\uFAD9'
    | '\uFB1D' | '\uFB1F'..'\uFB28' | '\uFB2A'..'\uFB36'
    | '\uFB38'..'\uFB3C' | '\uFB3E' | '\uFB40'..'\uFB41'
    | '\uFB43'..'\uFB44' | '\uFB46'..'\uFBB1' | '\uFBD3'..'\uFD3D'
    | '\uFD50'..'\uFD8F' | '\uFD92'..'\uFDC7' | '\uFDF0'..'\uFDFB'
    | '\uFE70'..'\uFE74' | '\uFE76'..'\uFEFC' | '\uFF66'..'\uFF6F'
    | '\uFF71'..'\uFF9D' | '\uFFA0'..'\uFFBE' | '\uFFC2'..'\uFFC7'
    | '\uFFCA'..'\uFFCF' | '\uFFD2'..'\uFFD7' | '\uFFDA'..'\uFFDC'
    ;
