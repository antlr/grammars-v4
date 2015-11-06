// PHP grammar by Ivan Kochurkin (KvanTTT), 2015.
// Used Phalanger grammar: https://github.com/DEVSENSE/Phalanger by Jakub Míšek (jakubmisek)
// and old php grammar by Tom Everett (teverett).
// Runtime: C#.
// Licence: MIT.

lexer grammar PHPLexer;

channels { PhpComments }

@lexer::members
{public bool AspTags = true;
bool ScriptTag;
bool StyleTag;
string HeredocIdentifier;
int PrevTokenType;
string HtmlNameText;
bool PhpScript;

public override IToken NextToken()
{
    IToken token = base.NextToken();

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
            PhpScript = false;
            token = new CommonToken(ScriptClose);
        }
        else
        {
            // Add semicolon to the end of statement if it is absente.
            // For example: <?php echo "Hello world" ?>
            if (PrevTokenType == SemiColon || PrevTokenType == Colon
                || PrevTokenType == OpenCurlyBracket || PrevTokenType == CloseCurlyBracket)
            {
                token = base.NextToken();
            }
            else
            {
                token = new CommonToken(SemiColon);
            }
        }
    }
    else if (token.Type == HtmlName)
    {
        HtmlNameText = token.Text;
    }
    else if (token.Type == HtmlDoubleQuoteString)
    {
        if (string.Equals(token.Text, "php", System.StringComparison.OrdinalIgnoreCase) &&
            string.Equals(HtmlNameText, "language"))
        {
            PhpScript = true;
        }
    }
    
    else if (_mode == HereDoc)
    {
        // Heredoc and Nowdoc syntax suuport: http://php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc
        switch (token.Type)
        {
            case StartHereDoc:
            case StartNowDoc:
                HeredocIdentifier = token.Text.Substring(3).Trim().Trim('\'');
                while (_input.La(1) == '\r' || _input.La(1) == '\n')
                    _input.Consume();
                break;

            case HereDocText:
                if (CheckHeredocEnd(token.Text))
                {
                    PopMode();
                    if (token.Text.Trim().EndsWith(";"))
                    {
                        token = new CommonToken(SemiColon);
                    }
                    else
                    {
                        token = base.NextToken();
                    }
                }
                break;
        }
    }
    else if (_mode == PHP)
    {
        if (_channel != Hidden)
        {
            PrevTokenType = token.Type;
        }
    }

    return token;
}

bool CheckHeredocEnd(string text)
{
    text = text.Trim();
    bool semi = text.Length > 0 ? text[text.Length - 1] == ';' : false;
    string identifier = semi ? text.Substring(0, text.Length - 1) : text;
    var result = string.Equals(identifier, HeredocIdentifier, System.StringComparison.Ordinal);
    return result;
}}

// '<?=' will be transformed to 'echo' token.
// '<?= "Hello world"; ?>' will be transformed to '<?php echo "Hello world"; ?>'
fragment PhpStartEchoFragment: '<' ('?' '=' | {AspTags}? '%' '=');
fragment PhpStartFragment:     '<' ('?' (P H P)? | {AspTags}? '%');
fragment Digit:                [0-9];
fragment HexDigit:             [a-fA-F0-9];
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
fragment Decimal:                   Digit+;
fragment Hex:                       '0' ('x'|'X') HexDigit+;
fragment Float:                     Digit* '.'? Digit+;
fragment SingleQuoteStringFragment: (~('\'' | '\\') | '\\' . )* ;
fragment DoubleQuoteStringFragment: (~('\\' | '"') | '\\' . )* ;
fragment String:                    ( '\\' EscapeCharacter | ~('\\'| '"') )*;
fragment A: [aA];
fragment B: [bB];
fragment C: [cC];
fragment D: [dD];
fragment E: [eE];
fragment F: [fF];
fragment G: [gG];
fragment H: [hH];
fragment I: [iI];
fragment J: [jJ];
fragment K: [kK];
fragment L: [lL];
fragment M: [mM];
fragment N: [nN];
fragment O: [oO];
fragment P: [pP];
fragment Q: [qQ];
fragment R: [rR];
fragment S: [sS];
fragment T: [tT];
fragment U: [uU];
fragment V: [vV];
fragment W: [wW];
fragment X: [xX];
fragment Y: [yY];
fragment Z: [zZ];

SeaWhitespace:  (' ' | '\t' | '\r'? '\n')+ -> channel(HIDDEN);
HtmlText:       ~[<#]+;
PHPStartEcho:   PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStart:       PhpStartFragment -> skip, pushMode(PHP);
HtmlScriptOpen: '<' 'script' { ScriptTag = true; } -> pushMode(INSIDE);
HtmlStyleOpen:  '<' 'style' { StyleTag = true; } -> pushMode(INSIDE);
HtmlComment:    '<' '!' '--' .*? '-->';
HtmlDtd:        '<' '!' .*? '>';
HtmlOpen:       '<' -> pushMode(INSIDE);
Shebang
    : { _input.La(-1) <= 0 || _input.La(-1) == '\r' || _input.La(-1) == '\n' }? '#' '!' ~[\r\n]*
    ;
NumberSign:     '#' ~[<]* -> more;

mode INSIDE;

PHPStartEchoInside: PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartInside:     PhpStartFragment -> skip, pushMode(PHP);
HtmlClose: '>' {
PopMode();
if (ScriptTag)
{
    if (!PhpScript)
    {
        PushMode(SCRIPT);
    }
    else
    {
        PushMode(PHP);
    }
    ScriptTag = false;
}
else if (StyleTag)
{
    PushMode(STYLE);
    StyleTag = false;
}
};
HtmlSlashClose: '/>' -> popMode;
HtmlSlash:      '/';
HtmlEquals:     '=';

HtmlStartQuoteString:       '\'' -> pushMode(HtmlQuoteStringMode);
HtmlStartDoubleQuoteString: '"' -> pushMode(HtmlDoubleQuoteStringMode);
HtmlHex:                    '#' HexDigit+ ;
HtmlDecimal:                Digit+;
HtmlSpace:                  [ \t\r\n]+ -> channel(HIDDEN);
HtmlName:                   NameStartChar NameChar*;

mode HtmlQuoteStringMode;

PHPStartEchoInsideQuoteString: PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartInsideQuoteString:     PhpStartFragment -> skip, pushMode(PHP);
HtmlEndQuoteString:            '\'' -> popMode;
HtmlQuoteString:               ~[<']+;

mode HtmlDoubleQuoteStringMode;

PHPStartEchoDoubleQuoteString: PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartDoubleQuoteString:     PhpStartFragment -> skip, pushMode(PHP);
HtmlEndDoubleQuoteString:      '"' -> popMode;
HtmlDoubleQuoteString:         ~[<"]+;

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

PHPEnd:             (('?' | {AspTags}? '%') '>') | {PhpScript}? '</script>';
Whitespace:         [ \t\r\n]+ -> skip;
MultiLineComment:   '/*' .*? '*/' -> channel(PhpComments);
SingleLineComment:  '//' -> skip, pushMode(SingleLineCommentMode);
ShellStyleComment:  '#' -> skip, pushMode(SingleLineCommentMode);

Abstract:           A B S T R A C T;
Array:              A R R A Y;
As:                 A S;
BinaryCast:         B I N A R Y;
BoolType:           B O O L E A N | B O O L;
BooleanConstant:    T R U E | F A L S E;
Break:              B R E A K;
Callable:           C A L L A B L E;
Case:               C A S E;
Catch:              C A T C H;
Class:              C L A S S;
Clone:              C L O N E;
Const:              C O N S T;
Continue:           C O N T I N U E;
Declare:            D E C L A R E;
Default:            D E F A U L T;
Do:                 D O;
DoubleCast:         R E A L;
DoubleType:         D O U B L E;
Echo:               E C H O;
Else:               E L S E;
ElseIf:             E L S E I F;
Empty:              E M P T Y;

EndDeclare:         E N D D E C L A R E;
EndFor:             E N D F O R;
EndForeach:         E N D F O R E A C H;
EndIf:              E N D I F;
EndSwitch:          E N D S W I T C H;
EndWhile:           E N D W H I L E;

Eval:               E V A L;
Exit:               D I E;
Extends:            E X T E N D S;
Final:              F I N A L;
Finally:            F I N A L L Y;
FloatCast:          F L O A T;
For:                F O R;
Foreach:            F O R E A C H;
Function:           F U N C T I O N;
Global:             G L O B A L;
Goto:               G O T O;
If:                 I F;
Implements:         I M P L E M E N T S;
Import:             I M P O R T;
Include:            I N C L U D E;
IncludeOnce:        I N C L U D E '_' O N C E;
InstanceOf:         I N S T A N C E O F;
InsteadOf:          I N S T E A D O F;
Int16Cast:          I N T '16';
Int64Type:          I N T '64';
Int8Cast:           I N T '8';
Interface:          I N T E R F A C E;
IntType:            I N T E G E R | I N T;
IsSet:              I S S E T;
List:               L I S T;
LogicalAnd:         A N D;
LogicalOr:          O R;
LogicalXor:         X O R;
Namespace:          N A M E S P A C E;
New:                N E W;
Null:               N U L L;
ObjectType:         O B J E C T;
Parent_:            P A R E N T;
Partial:            P A R T I A L;
Print:              P R I N T;
Private:            P R I V A T E;
Protected:          P R O T E C T E D;
Public:             P U B L I C;
Require:            R E Q U I R E;
RequireOnce:        R E Q U I R E '_' O N C E;
Resource:           R E S O U R C E;
Return:             R E T U R N;
Static:             S T A T I C;
StringType:         S T R I N G;
Switch:             S W I T C H;
Throw:              T H R O W;
Trait:              T R A I T;
Try:                T R Y;
Typeof:             C L R T Y P E O F;
Uint16Cast:         U I N T '16';
Uint32Cast:         U I N T;
Uint64Cast:         U I N T '64';
Uint8Cast:          U I N T '8';
UnicodeCast:        U N I C O D E;
Unset:              U N S E T;
Use:                U S E;
Var:                V A R;
While:              W H I L E;
Yield:              Y I E L D;

Get:                '__' G E T;
Set:                '__' S E T;
Call:               '__' C A L L;
CallStatic:         '__' C A L L S T A T I C;
Constructor:        '__' C O N S T R U C T;
Destruct:           '__' D E S T R U C T;
Wakeup:             '__' W A K E U P;
Sleep:              '__' S L E E P;
Autoload:           '__' A U T O L O A D;
IsSet__:            '__' I S S E T;
Unset__:            '__' U N S E T;
ToString__:         '__' T O S T R I N G;
Invoke:             '__' I N V O K E;
SetState:           '__' S E T '_' S T A T E;
Clone__:            '__' C L O N E;
DebugInfo:          '__' D E B U G I N F O;
Namespace__:        '__' N A M E S P A C E '__';
Class__:            '__' C L A S S '__';
Traic__:            '__' T R A I T '__';
Function__:         '__' F U N C T I O N '__';
Method__:           '__' M E T H O D '__';
Line__:             '__' L I N E '__';
File__:             '__' F I L E '__';
Dir__:              '__' D I R '__';

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
CloseCurlyBracket:  '}';
Comma:              ',';
Colon:              ':';
SemiColon:          ';';
Eq:                 '=';
DoubleQuote:        '"';
Quote:              '\'';
BackQuote:          '`';

Label:              ('a'..'z' | 'A'..'Z' | '_')  ('a'..'z' | 'A'..'Z' | '0'..'9' | '_')*;
VarName:            '$' Label;
Numeric:            Decimal | Hex;

Real: Float (('e'|'E') ('+'|'-')? (Float | Decimal))?;

EscapeCharacter:
    'n' | 'r' | 't' | 'v' | 'e' | 'f' | '\\' | '$' | '"' |
    'd' | 's' | '(' | ')' | 'w' |
    'E' |
    '0'..'7'+ |
    'x' HexDigit+ |
    'u' ('0'..'9'|'a'..'f')+;

BackQuoteString:   '`' String '`';
SingleQuoteString: '\'' SingleQuoteStringFragment '\'';
DoubleQuoteString: '"' DoubleQuoteStringFragment '"';

StartNowDoc
    : '<<<' [ \t]* '\''  Label '\''  { _input.La(1) == '\r' || _input.La(1) == '\n' }? -> pushMode(HereDoc)
    ;
StartHereDoc
    : '<<<' [ \t]* Label { _input.La(1) == '\r' || _input.La(1) == '\n' }? -> pushMode(HereDoc)
    ;

mode SingleLineCommentMode;

Comment: ~[\r\n?]+ -> channel(PhpComments);
PHPEndSingleLineComment: '?' '>';
CommentQuestionMark:     '?' -> type(Comment), channel(PhpComments);
CommentEnd: [\r\n] -> skip, popMode; // exit from comment.

mode HereDoc;

HereDocText: ~[\r\n]*? '\r'? '\n';
