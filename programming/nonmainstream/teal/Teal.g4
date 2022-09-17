// Project: https://github.com/teal-language/tl
grammar Teal;

chunk
    : block EOF
    ;

block
    : stat* retstat?
    ;

stat
    : ';'                                                                       # SemiStat
    | varlist '=' explist                                                       # AssignStat
    | functioncall                                                              # FuncCallStat
    | label                                                                     # LabelStat
    | 'break'                                                                   # BreakStat
    | 'goto' NAME                                                               # GotoStat
    | 'do' block 'end'                                                          # DoStat
    | 'while' exp 'do' block 'end'                                              # WhileStat
    | 'repeat' block 'until' exp                                                # RepeatStat
    | 'if' exp 'then' block ('elseif' exp 'then' block)* ('else' block)? 'end'  # IfStat
    | 'for' NAME '=' exp ',' exp (',' exp)? 'do' block 'end'                    # ForStat
    | 'for' namelist 'in' explist 'do' block 'end'                              # ForInStat
    | 'function' funcname funcbody                                              # FuncStat
    | 'local' 'function' NAME funcbody                                          # LocalFuncStat
    | 'local' attnamelist (':' typelist)? ('=' explist)?                        # LocalAttrAssignStat
    | 'local' NAME '=' newtype                                                  # LocalNewTypeStat
    | 'global' 'function' NAME funcbody                                         # GlobalFuncStat
    | 'global' attnamelist ':' typelist                                         # GlobalAttrStat
    | 'global' attnamelist (':' typelist)? '=' explist                          # GlobalAttrAssignStat
    | 'global' NAME '=' newtype                                                 # GlobalAssignStat
    ;

attnamelist
    : NAME attrib? (',' NAME attrib?)*
    ;

attrib
    : '<' NAME '>'
    ;

// we rename `type` to `typ` because of keyword `type`
typ
    : '(' typ ')'
    | basetype ('|' basetype)*
    ;

basetype
    : 'string' | 'boolean' | 'nil' | 'number'
    | '{' typ '}' | '{' typ ':' typ '}'
    | 'function' functiontype
    | NAME typeargs?
    ;

typelist
    : typ (',' typ)?
    ;

retlist
    : '(' typelist? '...'? ')'
    | typelist '...'?
    ;

typeargs
    : '<' NAME (',' NAME )* '>'
    ;

newtype
    : 'record' typeargs? ('{' typ '}')? (NAME '=' newtype)* (NAME ':' typ)* 'end'   # RecordNewType
    | 'enum' str* 'end'                                                             # EnumNewType
    | 'functiontype' functiontype                                                   # FuncNewType
    ;

functiontype
    : typeargs? '(' partypelist ')' (':' retlist)?
    ;

partypelist
    : partype (',' partype)*
    ;

partype
    : (NAME ':')? typ
    ;

parnamelist
    : parname (',' parname)*
    ;

parname
    : NAME (':' typ)?
    ;

retstat
    : 'return' explist? ';'?                                                    # ReturnStat
    ;

label
    : '::' NAME '::'
    ;

funcname
    : NAME ('.' NAME)* (':' NAME)?
    ;

varlist
    : variable (',' variable)*
    ;

namelist
    : NAME (',' NAME)*
    ;

explist
    : exp (',' exp)*
    ;

exp
    : 'nil' | 'false' | 'true'
    | number
    | str
    | '...'
    | functiondef
    | prefixexp
    | tableconstructor
    | exp 'as' typ
    | <assoc=right>  exp operatorPower exp
    | operatorUnary exp
    | exp operatorMulDivMod exp
    | exp operatorAddSub exp
    | <assoc=right> exp operatorStrcat exp
    | exp operatorComparison exp
    | NAME 'is' typ
    | exp operatorAnd exp
    | exp operatorOr exp
    | exp operatorBitwise exp
    ;

prefixexp
    : varOrExp nameAndArgs*
    ;

functioncall
    : varOrExp nameAndArgs+
    ;

varOrExp
    : variable | '(' exp ')'
    ;

// we rename `var` to `variable` because of keyword `var`
variable
    : (NAME | '(' exp ')' varSuffix) varSuffix*
    ;

varSuffix
    : nameAndArgs* ('[' exp ']' | '.' NAME)
    ;

nameAndArgs
    : (':' NAME)? args
    ;

args
    : '(' explist? ')' | tableconstructor | str
    ;

functiondef
    : 'function' funcbody
    ;

funcbody
    : typeargs? '(' parlist? ')' (':' retlist)? block 'end'
    ;

parlist
    : namelist (',' '...')? | '...'
    | parnamelist (',' '...' (':' typ)?)? | '...' (':' typ)?
    ;

tableconstructor
    : '{' fieldlist? '}'
    ;

fieldlist
    : field (fieldsep field)* fieldsep?
    ;

field
    : '[' exp ']' '=' exp                           # BracketAssginField
    | NAME (':' typ)? '=' exp                       # AssignField
    | NAME '=' newtype                              # AssignNewTypeField
    | exp                                           # ExprField
    ;

fieldsep
    : ',' | ';'
    ;

operatorOr
	: 'or';

operatorAnd
	: 'and';

operatorComparison
	: '<' | '>' | '<=' | '>=' | '~=' | '==';

operatorStrcat
	: '..';

operatorAddSub
	: '+' | '-';

operatorMulDivMod
	: '*' | '/' | '%' | '//';

operatorBitwise
	: '&' | '|' | '~' | '<<' | '>>';

operatorUnary
    : 'not' | '#' | '-' | '~';

operatorPower
    : '^';

number
    : INT | HEX | FLOAT | HEX_FLOAT
    ;

str
    : NORMALSTRING | CHARSTRING | LONGSTRING
    ;

// LEXER

NAME
    : [a-zA-Z_][a-zA-Z_0-9]*
    ;

NORMALSTRING
    : '"' ( EscapeSequence | ~('\\'|'"') )* '"'
    ;

CHARSTRING
    : '\'' ( EscapeSequence | ~('\''|'\\') )* '\''
    ;

LONGSTRING
    : '[' NESTED_STR ']'
    ;

fragment
NESTED_STR
    : '=' NESTED_STR '='
    | '[' .*? ']'
    ;

INT
    : Digit+
    ;

HEX
    : '0' [xX] HexDigit+
    ;

FLOAT
    : Digit+ '.' Digit* ExponentPart?
    | '.' Digit+ ExponentPart?
    | Digit+ ExponentPart
    ;

HEX_FLOAT
    : '0' [xX] HexDigit+ '.' HexDigit* HexExponentPart?
    | '0' [xX] '.' HexDigit+ HexExponentPart?
    | '0' [xX] HexDigit+ HexExponentPart
    ;

fragment
ExponentPart
    : [eE] [+-]? Digit+
    ;

fragment
HexExponentPart
    : [pP] [+-]? Digit+
    ;

fragment
EscapeSequence
    : '\\' [abfnrtvz"'\\]
    | '\\' '\r'? '\n'
    | DecimalEscape
    | HexEscape
    | UtfEscape
    ;

fragment
DecimalEscape
    : '\\' Digit
    | '\\' Digit Digit
    | '\\' [0-2] Digit Digit
    ;

fragment
HexEscape
    : '\\' 'x' HexDigit HexDigit
    ;

fragment
UtfEscape
    : '\\' 'u{' HexDigit+ '}'
    ;

fragment
Digit
    : [0-9]
    ;

fragment
HexDigit
    : [0-9a-fA-F]
    ;

// define doc lexer before comment lexer
COMMENT
    : '--[' NESTED_STR ']' -> channel(HIDDEN)
    ;

LINE_COMMENT
    : '--'
    (                                               // --
    | '[' '='*                                      // --[==
    | '[' '='* ~('='|'['|'\r'|'\n') ~('\r'|'\n')*   // --[==AA
    | ~('['|'\r'|'\n') ~('\r'|'\n')*                // --AAA
    ) ('\r\n'|'\r'|'\n'|EOF)
    -> channel(HIDDEN)
    ;

WS
    : [ \t\u000C\r\n]+ -> skip
    ;

SHEBANG
    : '#' '!' ~('\n'|'\r')* -> channel(HIDDEN)
    ;

