/*
PHP grammar.
The MIT License (MIT).
Copyright (c) 2015-2020, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.
Copyright (c) 2019, Thierry Marianne (thierry.marianne@weaving-the-web.org)
Copyright (c) 2019-2020, Student Main for php7, php8 support.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

lexer grammar PhpLexer;

channels { PhpComments, ErrorLexem, SkipChannel }

options {
    superClass=PhpLexerBase;
    caseInsensitive = true;
}

// Insert here @header for C++ lexer.

SeaWhitespace:  [ \t\r\n]+ -> channel(HIDDEN);
HtmlText:       ~[<#]+;
XmlStart:       '<?xml' -> pushMode(XML);
PHPStartEcho:   PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStart:       PhpStartFragment -> channel(SkipChannel), pushMode(PHP);
HtmlScriptOpen: '<script' { _scriptTag = true; } -> pushMode(INSIDE);
HtmlStyleOpen:  '<style' { _styleTag = true; } -> pushMode(INSIDE);
HtmlComment:    '<!' '--' .*? '-->' -> channel(HIDDEN);
HtmlDtd:        '<!' .*? '>';
HtmlOpen:       '<' -> pushMode(INSIDE);
Shebang
    : '#' { this.IsNewLineOrStart(-2) }? '!' ~[\r\n]*
    ;
NumberSign:     '#' ~'<'* -> more;
Error:          .         -> channel(ErrorLexem);

// TODO: parse xml attributes.
mode XML;

XmlText:                  ~'?'+;
XmlClose:                 '?>' -> popMode;
XmlText2:                 '?' -> type(XmlText);

mode INSIDE;

PHPStartEchoInside: PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartInside:     PhpStartFragment -> channel(SkipChannel), pushMode(PHP);
HtmlClose: '>' { this.PushModeOnHtmlClose(); };
HtmlSlashClose: '/>' -> popMode;
HtmlSlash:      '/';
HtmlEquals:     '=';

HtmlStartQuoteString:       '\\'? '\'' -> pushMode(HtmlQuoteStringMode);
HtmlStartDoubleQuoteString: '\\'? '"'  -> pushMode(HtmlDoubleQuoteStringMode);
HtmlHex:                    '#' HexDigit+ ;
HtmlDecimal:                Digit+;
HtmlSpace:                  [ \t\r\n]+ -> channel(HIDDEN);
HtmlName:                   HtmlNameStartChar HtmlNameChar*;
ErrorInside:                .          -> channel(ErrorLexem);

mode HtmlQuoteStringMode;

PHPStartEchoInsideQuoteString: PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartInsideQuoteString:     PhpStartFragment -> channel(SkipChannel), pushMode(PHP);
HtmlEndQuoteString:            '\'' '\''? -> popMode;
HtmlQuoteString:               ~[<']+;
ErrorHtmlQuote:                .          -> channel(ErrorLexem);

mode HtmlDoubleQuoteStringMode;

PHPStartEchoDoubleQuoteString: PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartDoubleQuoteString:     PhpStartFragment -> channel(SkipChannel), pushMode(PHP);
HtmlEndDoubleQuoteString:      '"' '"'? -> popMode;
HtmlDoubleQuoteString:         ~[<"]+;
ErrorHtmlDoubleQuote:          .          -> channel(ErrorLexem);

// Parse JavaScript with https://github.com/antlr/grammars-v4/tree/master/javascript if necessary.
// Php blocks can exist inside Script blocks too.
mode SCRIPT;

ScriptText:               ~'<'+;
// TODO: handle JS strings, but handle <?php tags inside them
//ScriptString:             '"' (~'"' | '\\' ('\r'? '\n' | .))* '"' -> type(ScriptText);
//ScriptString2:            '\'' (~'\'' | '\\' ('\r'? '\n' | .))* '\'' -> type(ScriptText);
HtmlScriptClose:          '</' 'script'? '>' -> popMode;
PHPStartInsideScriptEcho: PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartInsideScript:     PhpStartFragment -> channel(SkipChannel), pushMode(PHP);
ScriptText2:              '<' -> type(ScriptText);

mode STYLE;

StyleBody: .*? '</' 'style'? '>' -> popMode;

mode PHP;

PHPEnd:             ('?' | '%' {this.HasAspTags()}?) '>'
      |             '</script>' {this.HasPhpScriptTag()}?;
Whitespace:         [ \t\r\n]+ -> channel(SkipChannel);
MultiLineComment:   '/*' .*? '*/' -> channel(PhpComments);
SingleLineComment:  '//' -> channel(SkipChannel), pushMode(SingleLineCommentMode);
ShellStyleComment:  '#' -> channel(SkipChannel), pushMode(SingleLineCommentMode);

AttributeStart:     '#[';

Abstract:           'abstract';
Array:              'array';
As:                 'as';
BinaryCast:         'binary';
BoolType:           'bool' 'ean'?;
BooleanConstant:    'true'
               |    'false';
Break:              'break';
Callable:           'callable';
Case:               'case';
Catch:              'catch';
Class:              'class';
Clone:              'clone';
Const:              'const';
Continue:           'continue';
Declare:            'declare';
Default:            'default';
Do:                 'do';
DoubleCast:         'real';
DoubleType:         'double';
Echo:               'echo';
Else:               'else';
ElseIf:             'elseif';
Empty:              'empty';
Enum_:              'enum';

EndDeclare:         'enddeclare';
EndFor:             'endfor';
EndForeach:         'endforeach';
EndIf:              'endif';
EndSwitch:          'endswitch';
EndWhile:           'endwhile';

Eval:               'eval';
Exit:               'die';
Extends:            'extends';
Final:              'final';
Finally:            'finally';
FloatCast:          'float';
For:                'for';
Foreach:            'foreach';
Function_:           'function';
Global:             'global';
Goto:               'goto';
If:                 'if';
Implements:         'implements';
Import:             'import';
Include:            'include';
IncludeOnce:        'include_once';
InstanceOf:         'instanceof';
InsteadOf:          'insteadof';
Int8Cast:           'int8';
Int16Cast:          'int16';
Int64Type:          'int64';
IntType:            'int' 'eger'?;
Interface:          'interface';
IsSet:              'isset';
List:               'list';
LogicalAnd:         'and';
LogicalOr:          'or';
LogicalXor:         'xor';
Match_:             'match';
Namespace:          'namespace';
New:                'new';
Null:               'null';
ObjectType:         'object';
Parent_:            'parent';
Partial:            'partial';
Print:              'print';
Private:            'private';
Protected:          'protected';
Public:             'public';
Readonly:           'readonly';
Require:            'require';
RequireOnce:        'require_once';
Resource:           'resource';
Return:             'return';
Static:             'static';
StringType:         'string';
Switch:             'switch';
Throw:              'throw';
Trait:              'trait';
Try:                'try';
Typeof:             'clrtypeof';
UintCast:           'uint' ('8' | '16' | '64')?;
UnicodeCast:        'unicode';
Unset:              'unset';
Use:                'use';
Var:                'var';
While:              'while';
Yield:              'yield';
From:               'from';
LambdaFn:           'fn';
Ticks:              'ticks';
Encoding:           'encoding';
StrictTypes:        'strict_types';

Get:                '__get';
Set:                '__set';
Call:               '__call';
CallStatic:         '__callstatic';
Constructor:        '__construct';
Destruct:           '__destruct';
Wakeup:             '__wakeup';
Sleep:              '__sleep';
Autoload:           '__autoload';
IsSet__:            '__isset';
Unset__:            '__unset';
ToString__:         '__tostring';
Invoke:             '__invoke';
SetState:           '__set_state';
Clone__:            '__clone';
DebugInfo:          '__debuginfo';
Namespace__:        '__namespace__';
Class__:            '__class__';
Traic__:            '__trait__';
Function__:         '__function__';
Method__:           '__method__';
Line__:             '__line__';
File__:             '__file__';
Dir__:              '__dir__';

Spaceship:          '<=>';
Lgeneric:           '<:';
Rgeneric:           ':>';
DoubleArrow:        '=>';
Inc:                '++';
Dec:                '--';
IsIdentical:        '===';
IsNoidentical:      '!==';
IsEqual:            '==';
IsNotEq:            '<>'
       |            '!=';
IsSmallerOrEqual:   '<=';
IsGreaterOrEqual:   '>=';
PlusEqual:          '+=';
MinusEqual:         '-=';
MulEqual:           '*=';
Pow:                '**';
PowEqual:           '**=';
DivEqual:           '/=';
Concaequal:         '.=';
ModEqual:           '%=';
ShiftLeftEqual:     '<<=';
ShiftRightEqual:    '>>=';
AndEqual:           '&=';
OrEqual:            '|=';
XorEqual:           '^=';
BooleanOr:          '||';
BooleanAnd:         '&&';

NullCoalescing:     '??';
NullCoalescingEqual:'??=';

ShiftLeft:          '<<';
ShiftRight:         '>>';
DoubleColon:        '::';
ObjectOperator:     '->';
NamespaceSeparator: '\\';
Ellipsis:           '...';
Less:               '<';
Greater:            '>';
Ampersand:          '&';
Pipe:               '|';
Bang:               '!';
Caret:              '^';
Plus:               '+';
Minus:              '-';
Asterisk:           '*';
Percent:            '%';
Divide:             '/';
Tilde:              '~';
SuppressWarnings:   '@';
Dollar:             '$';
Dot:                '.';
QuestionMark:       '?';
OpenRoundBracket:   '(';
CloseRoundBracket:  ')';
OpenSquareBracket:  '[';
CloseSquareBracket: ']';
OpenCurlyBracket:   '{';
CloseCurlyBracket:  '}'
{ this.PopModeOnCurlyBracketClose(); };
Comma:              ',';
Colon:              ':';
SemiColon:          ';';
Eq:                 '=';
Quote:              '\'';
BackQuote:          '`';

VarName:            '$' NameString;
Label:              [a-z_][a-z_0-9]*;
Octal:              '0' 'o'? OctalDigit+ ('_' OctalDigit+)*;
Decimal:            '0' | NonZeroDigit Digit* ('_' Digit+)*;
Real:               (LNum '.' LNum? | LNum? '.' LNum ) ExponentPart?
    |               LNum+ ExponentPart;
Hex:                '0x' HexDigit+ ('_' HexDigit+)*;
Binary:             '0b' [01]+ ('_' [01]+)*;

BackQuoteString:   '`' ~'`'* '`';
SingleQuoteString: '\'' (~('\'' | '\\') | '\\' . )* '\'';
DoubleQuote:       '"' -> pushMode(InterpolationString);

StartNowDoc
    : '<<<' [ \t]* '\'' NameString '\''  { this.ShouldPushHereDocMode(1) }? -> pushMode(HereDoc)
    ;
StartHereDoc
    : '<<<' [ \t]* NameString { this.ShouldPushHereDocMode(1) }? -> pushMode(HereDoc)
    ;
ErrorPhp:                   .          -> channel(ErrorLexem);

mode InterpolationString;

VarNameInInterpolation:     '$' NameString                                      -> type(VarName); // TODO: fix such cases: "$people->john"
DollarString:               '$'                                                 -> type(StringPart);
CurlyDollar:                '{' { this.IsCurlyDollar(1) }? { this.SetInsideString(); }  -> channel(SkipChannel), pushMode(PHP);
CurlyString:                '{'                                                 -> type(StringPart);
EscapedChar:                '\\' .                                              -> type(StringPart);
DoubleQuoteInInterpolation: '"'                                                 -> type(DoubleQuote), popMode;
UnicodeEscape:              '\\u{' [a-z0-9][a-z0-9]+ '}';
StringPart:                 ~[${\\"]+;

mode SingleLineCommentMode;

Comment:                 ~[\r\n?]+ -> channel(PhpComments);
PHPEndSingleLineComment: '?' '>';
CommentQuestionMark:     '?' -> type(Comment), channel(PhpComments);
CommentEnd:              [\r\n] -> channel(SkipChannel), popMode; // exit from comment.

mode HereDoc;  // TODO: interpolation for heredoc strings.

HereDocText: ~[\r\n]*? ('\r'? '\n' | '\r');

// fragments.
// '<?=' will be transformed to 'echo' token.
// '<?= "Hello world"; ?>' will be transformed to '<?php echo "Hello world"; ?>'
fragment PhpStartEchoFragment: '<' ('?' '=' | { this.HasAspTags() }? '%' '=');
fragment PhpStartFragment:     '<' ('?' 'php'? | { this.HasAspTags() }? '%');
fragment NameString options { caseInsensitive = false; }: [a-zA-Z_\u0080-\ufffe][a-zA-Z0-9_\u0080-\ufffe]*;
fragment HtmlNameChar options { caseInsensitive = false; }
    : HtmlNameStartChar
    | '-'
    | '_'
    | '.'
    | Digit
    | '\u00B7'
    | '\u0300'..'\u036F'
    | '\u203F'..'\u2040'
    ;
fragment HtmlNameStartChar options { caseInsensitive = false; }
    : [:a-zA-Z]
    | '\u2070'..'\u218F'
    | '\u2C00'..'\u2FEF'
    | '\u3001'..'\uD7FF'
    | '\uF900'..'\uFDCF'
    | '\uFDF0'..'\uFFFD'
    ;
fragment LNum:                 Digit+ ('_' Digit+)*;
fragment ExponentPart:         'e' [+-]? LNum;
fragment NonZeroDigit:         [1-9];
fragment Digit:                [0-9];
fragment OctalDigit:           [0-7];
fragment HexDigit:             [a-f0-9];
