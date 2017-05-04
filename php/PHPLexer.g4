/*
PHP grammar.
The MIT License (MIT).
Copyright (c) 2015-2016, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.

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

lexer grammar PHPLexer;

channels { PhpComments, ErrorLexem }

@lexer::members
{public boolean AspTags = true;
boolean _scriptTag;
boolean _styleTag;
String _heredocIdentifier;
int _prevTokenType;
String _htmlNameText;
boolean _phpScript;
boolean _insideString;

@Override
public Token nextToken()
{
    CommonToken token = (CommonToken)super.nextToken();

    if (token.getType() == PHPEnd || token.getType() == PHPEndSingleLineComment)
    {
        if (_mode == SingleLineCommentMode)
        {
            // SingleLineCommentMode for such allowed syntax:
            // <?php echo "Hello world"; // comment ?>
            popMode(); // exit from SingleLineComment mode.
        }
        popMode(); // exit from PHP mode.
        
        if (token.getText().equals("</script>"))
        {
            _phpScript = false;
            token.setType(ScriptClose);
        }
        else
        {
            // Add semicolon to the end of statement if it is absente.
            // For example: <?php echo "Hello world" ?>
            if (_prevTokenType == SemiColon || _prevTokenType == Colon
                || _prevTokenType == OpenCurlyBracket || _prevTokenType == CloseCurlyBracket)
            {
                token = (CommonToken)super.nextToken();
            }
            else
            {
                token = new CommonToken(SemiColon);
            }
        }
    }
    else if (token.getType() == HtmlName)
    {
        _htmlNameText = token.getText();
    }
    else if (token.getType() == HtmlDoubleQuoteString)
    {
        if (token.getText().equals("php") && _htmlNameText.equals("language"))
        {
            _phpScript = true;
        }
    }
    else if (_mode == HereDoc)
    {
        // Heredoc and Nowdoc syntax support: http://php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc
        switch (token.getType())
        {
            case StartHereDoc:
            case StartNowDoc:
                _heredocIdentifier = token.getText().substring(3).trim().replace("\'","");
                break;

            case HereDocText:
                if (CheckHeredocEnd(token.getText()))
                {
                    popMode();
                    if (token.getText().trim().endsWith(";"))
                    {
                        token = new CommonToken(SemiColon);
                    }
                    else
                    {
                        token = (CommonToken)super.nextToken();
                    }
                }
                break;
        }
    }
    else if (_mode == PHP)
    {
        if (_channel != HIDDEN)
        {
            _prevTokenType = token.getType();
        }
    }

    return token;
}

boolean CheckHeredocEnd(String text)
{
    text = text.trim();
    boolean semi = (text.length() > 0) ? (text.charAt(text.length() - 1) == ';') : false;
    String identifier = semi ? text.substring(0, text.length() - 1) : text;
    boolean result = identifier.equals(_heredocIdentifier);
    return result;
}}

SeaWhitespace:  [ \t\r\n]+ -> channel(HIDDEN);
HtmlText:       ~[<#]+;
PHPStartEcho:   PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStart:       PhpStartFragment -> skip, pushMode(PHP);
HtmlScriptOpen: '<' 'script' { _scriptTag = true; } -> pushMode(INSIDE);
HtmlStyleOpen:  '<' 'style' { _styleTag = true; } -> pushMode(INSIDE);
HtmlComment:    '<' '!' '--' .*? '-->' -> channel(HIDDEN);
HtmlDtd:        '<' '!' .*? '>';
HtmlOpen:       '<' -> pushMode(INSIDE);
Shebang
    : { _input.LA(-1) <= 0 || _input.LA(-1) == '\r' || _input.LA(-1) == '\n' }? '#' '!' ~[\r\n]*
    ;
NumberSign:     '#' ~[<]* -> more;
Error:          .         -> channel(ErrorLexem);

mode INSIDE;

PHPStartEchoInside: PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartInside:     PhpStartFragment -> skip, pushMode(PHP);
HtmlClose: '>' {
popMode();
if (_scriptTag)
{
    if (!_phpScript)
    {
        pushMode(SCRIPT);
    }
    else
    {
        pushMode(PHP);
    }
    _scriptTag = false;
}
else if (_styleTag)
{
    pushMode(STYLE);
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
PHPStartInsideQuoteString:     PhpStartFragment -> skip, pushMode(PHP);
HtmlEndQuoteString:            '\'' '\''? -> popMode;
HtmlQuoteString:               ~[<']+;
ErrorHtmlQuote:                .          -> channel(ErrorLexem);

mode HtmlDoubleQuoteStringMode;

PHPStartEchoDoubleQuoteString: PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartDoubleQuoteString:     PhpStartFragment -> skip, pushMode(PHP);
HtmlEndDoubleQuoteString:      '"' '"'? -> popMode;
HtmlDoubleQuoteString:         ~[<"]+;
ErrorHtmlDoubleQuote:          .          -> channel(ErrorLexem);

// Parse JavaScript with https://github.com/antlr/grammars-v4/tree/master/ecmascript if necessary.
// Php blocks can exist inside Script blocks too.
mode SCRIPT;

ScriptText:               ~[<]+;
ScriptClose:              '<' '/' 'script'? '>' -> popMode;
PHPStartInsideScriptEcho: PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartInsideScript:     PhpStartFragment-> skip, pushMode(PHP);
ScriptText2:              '<' ~[<?/]* -> type(ScriptText);
ScriptText3:              '?' ~[<]* -> type(ScriptText);
ScriptText4:              '/' ~[<]* -> type(ScriptText);

mode STYLE;

StyleBody: .*? '</' 'style'? '>' -> popMode;

mode PHP;

PHPEnd:             (('?' | {AspTags}? '%') '>') | {_phpScript}? '</script>';
Whitespace:         [ \t\r\n]+ -> skip;
MultiLineComment:   '/*' .*? '*/' -> channel(PhpComments);
SingleLineComment:  '//' -> skip, pushMode(SingleLineCommentMode);
ShellStyleComment:  '#' -> skip, pushMode(SingleLineCommentMode);

Abstract:           'abstract';
Array:              'array';
As:                 'as';
BinaryCast:         'binary';
BoolType:           'boolean' | 'bool';
BooleanConstant:    'true' | 'false';
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
IsNotEq:            '<>' | '!=';
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
    skip();
    popMode();
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

BackQuoteString:   '`' ~'`'* '`';
SingleQuoteString: '\'' (~('\'' | '\\') | '\\' . )* '\'';
DoubleQuote:       '"' -> pushMode(InterpolationString);

StartNowDoc
    : '<<<' [ \t]* '\'' [a-zA-Z_][a-zA-Z_0-9]* '\''  { _input.LA(1) == '\r' || _input.LA(1) == '\n' }? -> pushMode(HereDoc)
    ;
StartHereDoc
    : '<<<' [ \t]* [a-zA-Z_][a-zA-Z_0-9]* { _input.LA(1) == '\r' || _input.LA(1) == '\n' }? -> pushMode(HereDoc)
    ;
ErrorPhp:                   .          -> channel(ErrorLexem);

mode InterpolationString;

VarNameInInterpolation:     '$' [a-zA-Z_][a-zA-Z_0-9]*                          -> type(VarName); // TODO: fix such cases: "$people->john"
DollarString:               '$'                                                 -> type(StringPart);
CurlyDollar:                '{' {_input.LA(1) == '$'}? {_insideString = true;}  -> skip, pushMode(PHP);
CurlyString:                '{'                                                 -> type(StringPart);
EscapedChar:                '\\' .                                              -> type(StringPart);
DoubleQuoteInInterpolation: '"'                                                 -> type(DoubleQuote), popMode;
StringPart:                 ~[${\\"]+;

mode SingleLineCommentMode;

Comment:                 ~[\r\n?]+ -> channel(PhpComments);
PHPEndSingleLineComment: '?' '>';
CommentQuestionMark:     '?' -> type(Comment), channel(PhpComments);
CommentEnd:              [\r\n] -> skip, popMode; // exit from comment.

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
