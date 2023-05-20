lexer grammar bcplLexer;

@members
{
bool IsNl()
{
	   var c = (this.InputStream as ChannelCommonTokenStream).LT(-1, 2);
	   var d = (this.InputStream as ChannelCommonTokenStream).LT(1, 2);
	   return c.Type == bcplParser.NL;
}
}

ACEQ : '&:=' ;
AND : '&' ;
ARROW : '->' ;
AT : '@' ;
BANG : '!' ;
CB : '$)' | '}';
CC : '::' ;
CEQ : ':=' ;
COLON : ':' ;
COMMA : ',' ;
DASH: '-' ;
DD : '..' ;
DOT : '.' ;
EQ : '=' ;
Eqv : 'EQV' ;
EQVCEQ : 'EQV:=' ;
False_ : 'FALSE' ;
GE : '>=' ;
GG : '>>' ;
GT : '>' ;
KABS : 'ABS' ;
KAND : 'AND' ;
KBE : 'BE' ;
KBREAK : 'BREAK' ;
KBY : 'BY' ;
KCASE : 'CASE' ;
KDEFAULT : 'DEFAULT' ;
KDO : 'DO' ;
KELSE : 'ELSE' ;
KENDCASE : 'ENDCASE' ;
KEQ : 'EQ' ;
KEVERY : 'EVERY' ;
KEXIT : 'EXIT' ;
KFINISH : 'FINISH' ;
KFIX : 'FIX' ;
KFLOAT : 'FLOAT' ;
KFLT : 'FLT' ;
KFOR : 'FOR' ;
KGET : 'GET' ;
KGLOBAL : 'GLOBAL' ;
KGOTO : 'GOTO' ;
KIF : 'IF' ;
KINTO : 'INTO' ;
KLET : 'LET' ;
KLOOP : 'LOOP' ;
KMANIFEST : 'MANIFEST' ;
KMATCH : 'MATCH' ;
KMOD : 'MOD' ;
KNE : 'NE' ;
KNEXT : 'NEXT' ;
KNOT : 'NOT' ;
KOF : 'OF' ;
KOR : 'OR' ;
KREPEAT : 'REPEAT' ;
KREPEATUNTIL : 'REPEATUNTIL' ;
KREPEATWHILE : 'REPEATWHILE' ;
KRESULTS : 'RESULTIS' ;
KRETURN : 'RETURN' ;
KSECTION : 'SECTION' ;
KSLCT : 'SLCT' ;
KSTATIC : 'STATIC' ;
KSWITCHON : 'SWITCHON' ;
KTEST : 'TEST' ;
KTHEN : 'THEN' ;
KTO : 'TO' ;
KUNLESS : 'UNLESS' ;
KUNTIL : 'UNTIL' ;
KVALOF : 'VALOF' ;
KVEC : 'VEC' ;
KWHILE : 'WHILE' ;
KXOR : 'XOR' ;
LBR : '[' ;
LE : '<=' ;
LL : '<<' ;
LLCEQ : '<<:=' ;
LP : '(' ;
LT : '<' ;
MODCEQ : 'MOD:=' ;
NCEQ : '~:=' ;
NE : '~=' ;
Neqv : 'NEQV' ;
NEQVCEQ : 'NEQV:=' ;
OB : '$(' | '{';
PABS : '#ABS' ;
PARROW : '#->' ;
PCEQ : '#:=' ;
PDD : '#..' ;
PE : '#=' ;
PERCENT : '%' ;
PGE : '#>=' ;
PGT : '#>' ;
PLCEQ : '+:=' ;
PLE : '#<=' ;
PLT : '#<' ;
PLUS : '+' ;
PMINUS : '#-' ;
PMOD : '#MOD' ;
PMODCEQ : '#MOD:=' ;
PNCEQ : '#~:=' ;
PNE : '#~=' ;
POP : '#(' ;
PPCEQ : '#+:=' ;
PPLUS : '#+' ;
PREM : '#REM' ;
PSCEQ : '#*:=' ;
PSLCEQ : '#/:=' ;
PSTAR : '#*' ;
QM : '?' ;
RBR : ']' ;
REM : 'REM' ;
RP : ')' ;
RRCEQ : '>>:=' ;
SCEQ : '*:=' ;
SEMI : ';' ;
SLASH: '/' ;
SLCEQ : '/:=' ;
STAR: '*' ;
Table : 'TABLE' ;
TILDE : '~' ;
True_ : 'TRUE' ;
VBAR : '|' ;
VCEQ : '|:=' ;
XORCEQ : 'XOR:=' ;


Left_dollar_open : OB ;

// 8.8.1 Identifier, Strings, Numbers.
fragment Letter : 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z'
// extended
 | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z'
 ;
fragment Octal_digit : '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' ;
fragment Hex_digit : '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' ;
fragment Digit : '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ;
// extended
String_constant : '"' ~'"'* /* 255 or fewer characters */ '"' ;
Character_constant : '\'' '*'? . '\'' ;
Octal_number : '#' Octal_digit Octal_digit* ;
Hex_number : ('#X'|'#x') Hex_digit Hex_digit* ;
Binary_number : ('#B'|'#b') [01]+ ;
Digits : Digit+;
Identifier : Letter (Letter | Digit | '.' | '_')* ;

Comment : ('/*' .*? '*/' | '//' ~('\n' | '\r')*) -> channel(HIDDEN) ;
WS : [ \t]+ -> channel(HIDDEN) ;
NL : [\n\r]+ -> channel(2) ;

