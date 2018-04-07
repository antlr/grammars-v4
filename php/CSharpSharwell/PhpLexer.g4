/*
PHP grammar.
The MIT License (MIT).
Copyright (c) 2015-2017, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.

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

@lexer::members
{public bool AspTags = true;
bool _scriptTag;
bool _styleTag;
string _heredocIdentifier;
int _prevTokenType;
string _htmlNameText;
bool _phpScript;
bool _insideString;

public override IToken NextToken()
{
    CommonToken token = (CommonToken)base.NextToken();

    if (token.Type == PHPEnd || token.Type == PHPEndSingleLineComment)
    {
        if (_mode == SingleLineCommentMode)
        {
            // SingleLineCommentMode for such allowed syntax:
            // <?php echo "Hello world"; // comment ?>
            PopMode(); // exit from SingleLineComment mode.
        }
        PopMode(); // exit from PHP mode.
        
        if (string.Equals(token.Text, "</script>", System.StringComparison.Ordinal))
        {
            _phpScript = false;
            token.Type = ScriptClose;
        }
        else
        {
            // Add semicolon to the end of statement if it is absente.
            // For example: <?php echo "Hello world" ?>
            if (_prevTokenType == SemiColon || _prevTokenType == Colon
                || _prevTokenType == OpenCurlyBracket || _prevTokenType == CloseCurlyBracket)
            {
                token.Channel = SkipChannel;
            }
            else
            {
                token.Type = SemiColon;
            }
        }
    }
    else if (token.Type == HtmlName)
    {
        _htmlNameText = token.Text;
    }
    else if (token.Type == HtmlDoubleQuoteString)
    {
        if (string.Equals(token.Text, "php", System.StringComparison.OrdinalIgnoreCase) &&
            string.Equals(_htmlNameText, "language"))
        {
            _phpScript = true;
        }
    }
    else if (_mode == HereDoc)
    {
        // Heredoc and Nowdoc syntax support: http://php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc
        switch (token.Type)
        {
            case StartHereDoc:
            case StartNowDoc:
                _heredocIdentifier = token.Text.Substring(3).Trim().Trim('\'');
                break;

            case HereDocText:
                if (CheckHeredocEnd(token.Text))
                {
                    PopMode();
                    if (token.Text.Trim().EndsWith(";"))
                    {
                        token.Type = SemiColon;
                    }
                    else
                    {
                        token.Channel = SkipChannel;
                    }
                }
                break;
        }
    }
    else if (_mode == PHP)
    {
        if (_channel != Hidden)
        {
            _prevTokenType = token.Type;
        }
    }

    return token;
}

bool CheckHeredocEnd(string text)
{
    text = text.Trim();
    bool semi = text.Length > 0 ? text[text.Length - 1] == ';' : false;
    string identifier = semi ? text.Substring(0, text.Length - 1) : text;
    var result = string.Equals(identifier, _heredocIdentifier, System.StringComparison.Ordinal);
    return result;
}}

SeaWhitespace:  [ \t\r\n]+ -> channel(HIDDEN);
HtmlText:       ~[<#]+;
XmlStart:       '<' '?' 'xml' -> pushMode(XML);
PHPStartEcho:   PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStart:       PhpStartFragment -> channel(SkipChannel), pushMode(PHP);
HtmlScriptOpen: '<' 'script' { _scriptTag = true; } -> pushMode(INSIDE);
HtmlStyleOpen:  '<' 'style' { _styleTag = true; } -> pushMode(INSIDE);
HtmlComment:    '<' '!' '--' .*? '-->' -> channel(HIDDEN);
HtmlDtd:        '<' '!' .*? '>';
HtmlOpen:       '<' -> pushMode(INSIDE);
Shebang
    : { _input.La(-1) <= 0 || _input.La(-1) == '\r' || _input.La(-1) == '\n' }? '#' '!' ~[\r\n]*
    ;
NumberSign:     '#' ~[<]* -> more;
Error:          .         -> channel(ErrorLexem);

mode INSIDE;

PHPStartEchoInside: PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartInside:     PhpStartFragment -> channel(SkipChannel), pushMode(PHP);
HtmlClose: '>' {
PopMode();
if (_scriptTag)
{
    if (!_phpScript)
    {
        PushMode(SCRIPT);
    }
    else
    {
        PushMode(PHP);
    }
    _scriptTag = false;
}
else if (_styleTag)
{
    PushMode(STYLE);
    _styleTag = false;
}
};
HtmlSlashClose: '/>' -> popMode;
HtmlSlash:      '/';
HtmlEquals:     '=';

HtmlStartQuoteString:       '\\'? '\'' -> pushMode(HtmlQuoteStringMode);
HtmlStartDoubleQuoteString: '\\'? '"'  -> pushMode(HtmlDoubleQuoteStringMode);
HtmlHex:                    '#' HexDigit+ ;
HtmlDecimal:                Digit+;
HtmlSpace:                  [ \t\r\n]+ -> channel(HIDDEN);
HtmlName:                   NameStartChar NameChar*;
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

// TODO: parse xml attributes.
mode XML;

XmlText:                  ~[?]+;
XmlClose:                 '?' '>' -> popMode;
XmlText2:                 '?' -> type(XmlText);

// Parse JavaScript with https://github.com/antlr/grammars-v4/tree/master/ecmascript if necessary.
// Php blocks can exist inside Script blocks too.
mode SCRIPT;

ScriptText:               ~[<]+;
ScriptClose:              '<' '/' 'script'? '>' -> popMode;
PHPStartInsideScriptEcho: PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartInsideScript:     PhpStartFragment -> channel(SkipChannel), pushMode(PHP);
ScriptText2:              '<' -> type(ScriptText);

mode STYLE;

StyleBody: .*? '</' 'style'? '>' -> popMode;

mode PHP;

PHPEnd:             (('?' | {AspTags}? '%') '>')
      |              {_phpScript}? '</script>';
Whitespace:         [ \t\r\n]+ -> channel(SkipChannel);
MultiLineComment:   '/*' .*? '*/' -> channel(PhpComments);
SingleLineComment:  '//' -> channel(SkipChannel), pushMode(SingleLineCommentMode);
ShellStyleComment:  '#' -> channel(SkipChannel), pushMode(SingleLineCommentMode);

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
Function:           'function';
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
{
if (_insideString)
{
    _insideString = false;
    Channel = SkipChannel;
    PopMode();
}
};
Comma:              ',';
Colon:              ':';
SemiColon:          ';';
Eq:                 '=';
Quote:              '\'';
BackQuote:          '`';

VarName:            '$' [a-zA-Z_][a-zA-Z_0-9]*;
Label:              [a-zA-Z_][a-zA-Z_0-9]*;
Octal:              '0' [0-7]+;
Decimal:            Digit+;
Real:               (Digit+ '.' Digit* | '.' Digit+) ExponentPart? | Digit+ ExponentPart;
Hex:                '0x' HexDigit+;
Binary:             '0b' [01]+;

BackQuoteString:    '`' ~'`'* '`';
SingleQuoteString:  '\'' (~('\'' | '\\') | '\\' . )* '\'';
DoubleQuote:        '"' -> pushMode(InterpolationString);

StartNowDoc:        '<<<' [ \t]* '\'' [a-zA-Z_][a-zA-Z_0-9]* '\''  { _input.La(1) == '\r' || _input.La(1) == '\n' }? -> pushMode(HereDoc);
StartHereDoc:       '<<<' [ \t]* [a-zA-Z_][a-zA-Z_0-9]* { _input.La(1) == '\r' || _input.La(1) == '\n' }? -> pushMode(HereDoc);
ErrorPhp:           .          -> channel(ErrorLexem);

mode InterpolationString;

VarNameInInterpolation:     '$' [a-zA-Z_][a-zA-Z_0-9]*                          -> type(VarName); // TODO: fix such cases: "$people->john"
DollarString:               '$'                                                 -> type(StringPart);
CurlyDollar:                '{' {_input.La(1) == '$'}? {_insideString = true;}  -> channel(SkipChannel), pushMode(PHP);
CurlyString:                '{'                                                 -> type(StringPart);
EscapedChar:                '\\' .                                              -> type(StringPart);
DoubleQuoteInInterpolation: '"'                                                 -> type(DoubleQuote), popMode;
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
fragment PhpStartEchoFragment: '<' ('?' '=' | {AspTags}? '%' '=');
fragment PhpStartFragment:     '<' ('?' 'php'? | {AspTags}? '%');
fragment NameChar
    : NameStartChar
    | '-'
    | '_'
    | '.'
    | Digit
    | '\u00B7'
    | '\u0300'..'\u036F'
    | '\u203F'..'\u2040'
    ;
fragment NameStartChar
    : [:a-zA-Z]
    | '\u2070'..'\u218F'
    | '\u2C00'..'\u2FEF'
    | '\u3001'..'\uD7FF'
    | '\uF900'..'\uFDCF'
    | '\uFDF0'..'\uFFFD'
    ;
fragment ExponentPart:         'e' [+-]? Digit+;
fragment Digit:                [0-9];
fragment HexDigit:             [a-fA-F0-9];