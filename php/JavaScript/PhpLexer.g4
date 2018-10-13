/*
 PHP grammar.
 
 The MIT License (MIT). Copyright (c) 2015-2017, Ivan Kochurkin (kvanttt@gmail.com), Positive
 Technologies.
 
 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 associated documentation files (the "Software"), to deal in the Software without restriction,
 including without limitation the rights to use, copy, modify, merge, publish, distribute,
 sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:
 
 The above copyright notice and this permission notice shall be included in all copies or
 substantial portions of the Software.
 
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

lexer grammar PhpLexer;

@header {
    let CommonToken = require('antlr4/Token').CommonToken;
}

channels {
	PhpComments,
	ErrorLexem
}

@lexer::members {
    const old_lexer = PhpLexer;
    PhpLexer = function() {
        old_lexer.apply(this, arguments);
        this.AspTags            = true;
        this._scriptTag         = false;
        this._styleTag          = false;
        this._heredocIdentifier = undefined;
        this._prevTokenType     = 0;
        this._htmlNameText      = undefined;
        this._phpScript         = false;
        this._insideString      = false;
    }

    PhpLexer.prototype = Object.create(old_lexer.prototype);
    PhpLexer.prototype.constructor = PhpLexer;

    PhpLexer.prototype.nextToken = function() {
        let token = antlr4.Lexer.prototype.nextToken.call(this);

        if (token.type === this.PHPEnd || token.type === this.PHPEndSingleLineComment) {
            if (this._mode === this.SingleLineCommentMode) {
                // SingleLineCommentMode for such allowed syntax:
                // // <?php echo "Hello world"; // comment ?>
                this.popMode();
            }
            this.popMode();

            if (token.text === "</script>") {
                this._phpScript = false;
                token.type = this.ScriptClose;
            } else {
                // Add semicolon to the end of statement if it is absent.
                // For example: <?php echo "Hello world" ?>
                if (this._prevTokenType === this.SemiColon || this._prevTokenType === this.Colon || this._prevTokenType === this.OpenCurlyBracket || this._prevTokenType === this.CloseCurlyBracket) {
                    token = antlr4.Lexer.prototype.nextToken.call(this);
                } else {
                    token = CommonToken(type=this.SemiColon);
                    token.text = ';';
                }
            }
        }

        else if (token.type === this.HtmlName) {
            this._htmlNameText = token.text
        }

        else if (token.type === this.HtmlDoubleQuoteString) {
            if (token.text === "php" && this._htmlNameText === "language") {
                this._phpScript = true;
            }
        }

        else if (this._mode === this.HereDoc) {
            // Heredoc and Nowdoc syntax support: http://php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc
            if (token.type === this.StartHereDoc || token.type === this.StartNowDoc) {
                this._heredocIdentifier = token.text.slice(3).trim().replace(/\'$/, '');
            }

            if (token.type === this.HereDocText) {
                if (this.CheckHeredocEnd(token.text)) {
                    this.popMode()
                    if (token.text.trim().endsWith(';')) {
                        token = CommonToken(type=this.SemiColon);
                        token.text = ';';
                    } else {
                        token = antlr4.Lexer.prototype.nextToken.call(this);
                    }
                }
            }
        }

        else if (this._mode === this.PHP) {
            if (this._channel === this.HIDDEN) {
                this._prevTokenType = token.type;
            }
        }

        return token;
    };

    PhpLexer.prototype.CheckHeredocEnd = function(text) {
        const identifier = text.trim().replace(/\;$/, "");
        return identifier === _heredocIdentifier;
    };

}

SeaWhitespace:[ \t\r\n]+ -> skip;
HtmlText: ~[<#]+;
PHPStartEcho:
	PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStart: PhpStartFragment -> skip, pushMode(PHP);
HtmlScriptOpen:
	'<' 'script' { this._scriptTag = true } -> pushMode(INSIDE);
HtmlStyleOpen:
	'<' 'style' { this._styleTag = true } -> pushMode(INSIDE);
HtmlComment: '<' '!' '--' .*? '-->' -> skip;
HtmlDtd: '<' '!' .*? '>';
HtmlOpen: '<' -> pushMode(INSIDE);
Shebang:
	'#' { this._input.LA(-2) <= 0 || this._input.LA(-2) == '\r'.charCodeAt(0) || this._input.LA(-2) == '\n'.charCodeAt(0)
	    }? '!' ~[\r\n]*;
NumberSign: '#' ~[<]* -> more;
Error: . -> channel(ErrorLexem);

mode INSIDE;

PHPStartEchoInside:
	PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartInside: PhpStartFragment -> skip, pushMode(PHP);
HtmlClose:
	'>' {
    this.popMode();
    if (this._scriptTag) {
        if (!this._phpScript) {
            this.pushMode(this.SCRIPT);
        } else {
            this.pushMode(this.PHP);
        }
        this._scriptTag = false;
    } else if (this._styleTag) {
        this.pushMode(this.STYLE);
        this._styleTag = false;
    }
};
HtmlSlashClose: '/>' -> popMode;
HtmlSlash: '/';
HtmlEquals: '=';

HtmlStartQuoteString:
	'\\'? '\'' -> pushMode(HtmlQuoteStringMode);
HtmlStartDoubleQuoteString:
	'\\'? '"' -> pushMode(HtmlDoubleQuoteStringMode);
HtmlHex: '#' HexDigit+;
HtmlDecimal: Digit+;
HtmlSpace: [ \t\r\n]+ -> skip;
HtmlName: NameStartChar NameChar*;
ErrorInside: . -> channel(ErrorLexem);

mode HtmlQuoteStringMode;

PHPStartEchoInsideQuoteString:
	PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartInsideQuoteString:
	PhpStartFragment -> skip, pushMode(PHP);
HtmlEndQuoteString: '\'' '\''? -> popMode;
HtmlQuoteString: ~[<']+;
ErrorHtmlQuote: . -> channel(ErrorLexem);

mode HtmlDoubleQuoteStringMode;

PHPStartEchoDoubleQuoteString:
	PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartDoubleQuoteString:
	PhpStartFragment -> skip, pushMode(PHP);
HtmlEndDoubleQuoteString: '"' '"'? -> popMode;
HtmlDoubleQuoteString: ~[<"]+;
ErrorHtmlDoubleQuote: . -> channel(ErrorLexem);

// Parse JavaScript with https://github.com/antlr/grammars-v4/tree/master/ecmascript if necessary.
// Php blocks can exist inside Script blocks too.
mode SCRIPT;

ScriptText: ~[<]+;
ScriptClose: '<' '/' 'script'? '>' -> popMode;
PHPStartInsideScriptEcho:
	PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartInsideScript: PhpStartFragment -> skip, pushMode(PHP);
ScriptText2: '<' ~[<?/]* -> type(ScriptText);
ScriptText3: '?' ~[<]* -> type(ScriptText);
ScriptText4: '/' ~[<]* -> type(ScriptText);

mode STYLE;

StyleBody: .*? '</' 'style'? '>' -> popMode;

mode PHP;

PHPEnd: ('?' | '%' {this.AspTags}?) '>' '</script>' {this._phpScript}?;

Whitespace: [ \t\r\n]+ -> skip;
MultiLineComment: '/*' .*? '*/' -> channel(PhpComments);
SingleLineComment:
	'//' -> skip, pushMode(SingleLineCommentMode);
ShellStyleComment:
	'#' -> skip, pushMode(SingleLineCommentMode);

Abstract: 'abstract';
Array: 'array';
As: 'as';
BinaryCast: 'binary';
BoolType: 'boolean' | 'bool';
BooleanConstant: 'true' | 'false';
Break: 'break';
Callable: 'callable';
Case: 'case';
Catch: 'catch';
Class: 'class';
Clone: 'clone';
Const: 'const';
Continue: 'continue';
Declare: 'declare';
Default: 'default';
Do: 'do';
DoubleCast: 'real';
DoubleType: 'double';
Echo: 'echo';
Else: 'else';
ElseIf: 'elseif';
Empty: 'empty';

EndDeclare: 'enddeclare';
EndFor: 'endfor';
EndForeach: 'endforeach';
EndIf: 'endif';
EndSwitch: 'endswitch';
EndWhile: 'endwhile';

Eval: 'eval';
Exit: 'die';
Extends: 'extends';
Final: 'final';
Finally: 'finally';
FloatCast: 'float';
For: 'for';
Foreach: 'foreach';
Function: 'function';
Global: 'global';
Goto: 'goto';
If: 'if';
Implements: 'implements';
Import: 'import';
Include: 'include';
IncludeOnce: 'include_once';
InstanceOf: 'instanceof';
InsteadOf: 'insteadof';
Int8Cast: 'int8';
Int16Cast: 'int16';
Int64Type: 'int64';
IntType: 'int' 'eger'?;
Interface: 'interface';
IsSet: 'isset';
List: 'list';
LogicalAnd: 'and';
LogicalOr: 'or';
LogicalXor: 'xor';
Namespace: 'namespace';
New: 'new';
Null: 'null';
ObjectType: 'object';
Parent_: 'parent';
Partial: 'partial';
Print: 'print';
Private: 'private';
Protected: 'protected';
Public: 'public';
Require: 'require';
RequireOnce: 'require_once';
Resource: 'resource';
Return: 'return';
Static: 'static';
StringType: 'string';
Switch: 'switch';
Throw: 'throw';
Trait: 'trait';
Try: 'try';
Typeof: 'clrtypeof';
UintCast: 'uint' ('8' | '16' | '64')?;
UnicodeCast: 'unicode';
Unset: 'unset';
Use: 'use';
Var: 'var';
While: 'while';
Yield: 'yield';

Get: '__get';
Set: '__set';
Call: '__call';
CallStatic: '__callstatic';
Constructor: '__construct';
Destruct: '__destruct';
Wakeup: '__wakeup';
Sleep: '__sleep';
Autoload: '__autoload';
IsSet__: '__isset';
Unset__: '__unset';
ToString__: '__tostring';
Invoke: '__invoke';
SetState: '__set_state';
Clone__: '__clone';
DebugInfo: '__debuginfo';
Namespace__: '__namespace__';
Class__: '__class__';
Traic__: '__trait__';
Function__: '__function__';
Method__: '__method__';
Line__: '__line__';
File__: '__file__';
Dir__: '__dir__';

Lgeneric: '<:';
Rgeneric: ':>';
DoubleArrow: '=>';
Inc: '++';
Dec: '--';
IsIdentical: '===';
IsNoidentical: '!==';
IsEqual: '==';
IsNotEq: '<>' | '!=';
IsSmallerOrEqual: '<=';
IsGreaterOrEqual: '>=';
PlusEqual: '+=';
MinusEqual: '-=';
MulEqual: '*=';
Pow: '**';
PowEqual: '**=';
DivEqual: '/=';
Concaequal: '.=';
ModEqual: '%=';
ShiftLeftEqual: '<<=';
ShiftRightEqual: '>>=';
AndEqual: '&=';
OrEqual: '|=';
XorEqual: '^=';
BooleanOr: '||';
BooleanAnd: '&&';
ShiftLeft: '<<';
ShiftRight: '>>';
DoubleColon: '::';
ObjectOperator: '->';
NamespaceSeparator: '\\';
Ellipsis: '...';
Less: '<';
Greater: '>';
Ampersand: '&';
Pipe: '|';
Bang: '!';
Caret: '^';
Plus: '+';
Minus: '-';
Asterisk: '*';
Percent: '%';
Divide: '/';
Tilde: '~';
SuppressWarnings: '@';
Dollar: '$';
Dot: '.';
QuestionMark: '?';
OpenRoundBracket: '(';
CloseRoundBracket: ')';
OpenSquareBracket: '[';
CloseSquareBracket: ']';
OpenCurlyBracket: '{';
CloseCurlyBracket:
	'}' {
if (this._insideString) {
    this._insideString = false;
    this.skip;
    this.popMode();
}
};
Comma: ',';
Colon: ':';
SemiColon: ';';
Eq: '=';
Quote: '\'';
BackQuote: '`';

VarName: '$' [a-zA-Z_][a-zA-Z_0-9]*;
Label: [a-zA-Z_][a-zA-Z_0-9]*;
Octal: '0' [0-7]+;
Decimal: Digit+;
Real: (Digit+ '.' Digit* | '.' Digit+) ExponentPart?
	| Digit+ ExponentPart;
Hex: '0x' HexDigit+;
Binary: '0b' [01]+;

BackQuoteString: '`' ~'`'* '`';
SingleQuoteString: '\'' (~('\'' | '\\') | '\\' .)* '\'';
DoubleQuote: '"' -> pushMode(InterpolationString);

StartNowDoc:
	'<<<' [ \t]* '\'' [a-zA-Z_][a-zA-Z_0-9]* '\'' { this._input.LA(1) === '\r'.charCodeAt(0) || this._input.LA(1) === '\n'.charCodeAt(0)
		}? -> pushMode(HereDoc);
StartHereDoc:
	'<<<' [ \t]* [a-zA-Z_][a-zA-Z_0-9]* { this._input.LA(1) === '\r'.charCodeAt(0) || this._input.LA(1) === '\n'.charCodeAt(0)
		}? -> pushMode(HereDoc);
ErrorPhp: . -> channel(ErrorLexem);

mode InterpolationString;

VarNameInInterpolation:
	'$' [a-zA-Z_][a-zA-Z_0-9]* -> type(VarName); // TODO: fix such cases: "$people->john"
DollarString: '$' -> type(StringPart);
CurlyDollar:
	'{' {this._input.LA(1) === '$'.charCodeAt(0)}? {this._insideString = true} -> channel( HIDDEN),
		pushMode(PHP);
CurlyString: '{' -> type(StringPart);
EscapedChar: '\\' . -> type(StringPart);
DoubleQuoteInInterpolation: '"' -> type(DoubleQuote), popMode;
StringPart: ~[${\\"]+;

mode SingleLineCommentMode;

Comment: ~[\r\n?]+ -> channel(PhpComments);
PHPEndSingleLineComment: '?' '>';
CommentQuestionMark:
	'?' -> type(Comment), channel(PhpComments);
CommentEnd: [\r\n] -> skip, popMode; // exit from comment.

mode HereDoc; // TODO: interpolation for heredoc strings.

HereDocText: ~[\r\n]*? ('\r'? '\n' | '\r');

// fragments. '<?=' will be transformed to 'echo' token. '<?= "Hello world"; ?>' will be transformed
// to '<?php echo "Hello world"; ?>'
fragment PhpStartEchoFragment:
	'<' ('?' '=' | {this.AspTags}? '%' '=');
fragment PhpStartFragment:
	'<' ('?' 'php'? | {this.AspTags}? '%');
fragment NameChar:
	NameStartChar
	| '-'
	| '_'
	| '.'
	| Digit
	| '\u00B7'
	| '\u0300' ..'\u036F'
	| '\u203F' ..'\u2040';
fragment NameStartChar:
	[:a-zA-Z]
	| '\u2070' ..'\u218F'
	| '\u2C00' ..'\u2FEF'
	| '\u3001' ..'\uD7FF'
	| '\uF900' ..'\uFDCF'
	| '\uFDF0' ..'\uFFFD';
fragment ExponentPart: 'e' [+-]? Digit+;
fragment Digit: [0-9];
fragment HexDigit: [a-fA-F0-9];
