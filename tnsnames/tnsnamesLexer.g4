// -------------------------------------------------------------------
// Definition of a grammar to parse a tnsnames.ora file.
// Specification as per Oracle 11g Release 2 Network Reference manual
// http://docs.oracle.com/cd/E11882_01/network.112/e10835/tnsnames.htm
// -------------------------------------------------------------------
// Norman Dunbar.
// Email: norman@dunbar-it.co.uk
// August 2014.
// -------------------------------------------------------------------
// Warning: I'm not a compiler writer, nor do I play one on TV.
// Warning: This is my first "proper" ANTLR grammar.
// -------------------------------------------------------------------
// This grammar assumes, that we are dealing with tnsnames entries that 
// locate a database, or, those that describe a listener or scan 
// listener.
// -------------------------------------------------------------------
// MAYBE TODO:
//
// PROTOCOL_STACK??? - I've never seen this in the wild! Only on
// http://www.toadworld.com/platforms/oracle/w/wiki/5484.defining-tnsname-addresses.aspx
//
// Add IP V6 lever rule. Currently only copes with IP V4.
//--------------------------------------------------------------------


//--------------------------------------------------------------------
// The MIT License (MIT) 
// 
// Copyright (c) 2014 by Norman Dunbar 
// 
// Permission is hereby granted, free of charge, to any person 
// obtaining a copy of this software and associated documentation 
// files (the "Software"), to deal in the Software without 
// restriction, including without limitation the rights to use, 
// copy, modify, merge, publish, distribute, sublicense, and/or sell 
// copies of the Software, and to permit persons to whom the 
// Software is furnished to do so, subject to the following 
// conditions: 
//
// The above copyright notice and this permission notice shall be 
// included in all copies or substantial portions of the Software. 
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
// OTHER DEALINGS IN THE SOFTWARE. 
//
// Project      : Oracle Tnsnames.ora parser grammer for ANTLR4.
// Developed by : Norman Dunbar, norman@dunbar-it.co.uk
//--------------------------------------------------------------------



lexer grammar tnsnamesLexer;

// ---------------------------------------------------------------
// Lexer rules. Lots of useful and interesting stuff happening
// around here.
// ---------------------------------------------------------------

L_PAREN          : '(' ;
                 
R_PAREN          : ')' ;
                 
L_SQUARE         : '[' ;
                 
R_SQUARE         : ']' ;
                 
EQUAL            : '=' ;
                 
DOT              : '.' ;   
                 
COMMA            : ',' ;

D_QUOTE          : '"' ;

S_QUOTE          : '\'' ;
                 
CONNECT_DATA     : C O N N E C T '_' D A T A ;

DESCRIPTION_LIST : DESCRIPTION '_' LIST ;
                 
DESCRIPTION      : D E S C R I P T I O N ;
                 
ADDRESS_LIST     : ADDRESS '_' LIST ;
                 
ADDRESS          : A D D R E S S ;
                 
PROTOCOL         : P R O T O C O L ;
                 
TCP              : T C P ;
                 
HOST             : H O S T ;
                 
PORT             : P O R T ;

LOCAL            : L O C A L ;

// ---------------------------------------------------------------
// Ok, I know this defines an IP version 4 address, but I haven't
// got my head around the IPv6 format yet!
// It seems that an IPv4 address that begins with a zero is octal.
// With leading "0x" or "0X" it's hexadecimal. Sigh.
// ---------------------------------------------------------------
IP               : QUAD DOT QUAD DOT QUAD DOT QUAD+ ;
                 
YES_NO           : Y E S | N O ;
                 
ON_OFF           : O N | O F F ;
                 
TRUE_FALSE       : T R U E | F A L S E ;  
                 
COMMENT          : '#' (.)*? '\n' -> skip ;
                 
INT              : DIGIT+ ;

OK               : O K ;
                 
DEDICATED        : D E D I C A T E D ;
                 
SHARED           : S H A R E D ;
                 
POOLED           : P O O L E D ;    
                 
LOAD_BALANCE     : L O A D '_' B A L A N C E ;   
                 
FAILOVER         : F A I L O V E R ;     
                 
UR               : U R ;
                 
UR_A             : A ;      
                 
ENABLE           : E N A B L E ;
                 
BROKEN           : B R O K E N ;
                 
SDU              : S D U ;
                 
RECV_BUF         : R E C V '_' BUF_SIZE ;
                 
SEND_BUF         : S E N D '_' BUF_SIZE ;
                 
SOURCE_ROUTE     : S O U R C E '_' R O U T E ;
                 
SERVICE          : S E R V I C E ;
                 
SERVICE_TYPE     : T Y P E '_' O F '_' SERVICE ;
                 
KEY              : K E Y ;
                 
IPC              : I P C ;

SPX              : S P X ;

NMP              : N M P ;

BEQ              : B E Q ;

PIPE             : P I P E ;

PROGRAM          : P R O G R A M ;

ARGV0            : A R G V '0' ;

ARGS             : A R G S ;
                 
SECURITY         : S E C U R I T Y ;
                 
SSL_CERT         : S S L '_' SERVER '_' C E R T '_' D N ;
                 
CONN_TIMEOUT     : C O N N E C T '_' T I M E O U T ;
                 
RETRY_COUNT      : R E T R Y '_' C O U N T ;
                 
TCT              : T R A N S P O R T '_' CONN_TIMEOUT ; 

//----------------------------------------------------------------------------
// Because IFILEs accept double, single or unquoted strings, we need
// special processing or there is carnage. When we find an IFILE, change
// to an IFILE processing mode - see the end of this file for the tokens etc.
//----------------------------------------------------------------------------
IFILE            : I F I L E -> pushMode(IFILE_MODE);

                 
// ---------------------------------------------------------------
// It seems I can't use D_QUOTE in the middle of the following 
// lexer rule. Compiling the grammar gives "rule reference D_QUOTE 
// is not currently supported in a set".
// ---------------------------------------------------------------
DQ_STRING        : D_QUOTE (~'"')* D_QUOTE ;


                 
//-------------------------------------------------
// CONNECT_DATA parameters.
//-------------------------------------------------
SERVICE_NAME     : SERVICE '_' NAME ;
                 
SID              : S I D ;
                 
INSTANCE_NAME    : I N S T A N C E '_' NAME ;       
                 
FAILOVER_MODE    : FAILOVER '_' M O D E ;
                 
GLOBAL_NAME      : G L O B A L '_' NAME ;
                 
HS               : H S ;
                 
RDB_DATABASE     : R D B '_' D A T A B A S E ;
                 
SERVER           : S E R V E R ;
                 
//-------------------------------------------------
// FAILOVER_MODE parameters.
//-------------------------------------------------
BACKUP           : B A C K U P ;
                 
TYPE             : T Y P E ;
                 
SESSION          : S E S S I O N ;
                 
SELECT           : S E L E C T ;
                 
NONE             : N O N E ;
                 
METHOD           : M E T H O D ;
                 
BASIC            : B A S I C ;
                 
PRECONNECT       : P R E C O N N E C T ;
                 
RETRIES          : R E T R I E S ;
                 
DELAY            : D E L A Y ;                 



//-------------------------------------------------
// IPv4 dotted Quads. For host IP addresses.
//-------------------------------------------------
QUAD             : '0'[xX] HEX_DIGIT+
                 | '0' OCT_DIGIT+
                 | INT 
                 ;


//-------------------------------------------------
// Other lexer rules, and fragments.
//-------------------------------------------------
ID               : [A-Za-z0-9][A-Za-z0-9_\-.]* ;
WS               : [ \t\r\n]+ -> skip ;



// ----------
// Fragments.
// ----------

fragment
A                : [Aa] ;
                 
fragment
B                : [Bb] ;
                 
fragment
C                : [Cc] ;
                 
fragment
D                : [Dd] ;
                 
fragment
E                : [Ee] ;
                 
fragment
F                : [Ff] ;
                 
fragment
G                : [Gg] ;
                 
fragment
H                : [Hh] ;
                 
fragment
I                : [Ii] ;
                 
fragment
J                : [Jj] ;
                 
fragment
K                : [Kk] ;
                 
fragment
L                : [Ll] ;
                 
fragment
M                : [Mm] ;
                 
fragment
N                : [Nn] ;
                 
fragment
O                : [Oo] ;
                 
fragment
P                : [Pp] ;
                 
fragment
Q                : [Qq] ;
                 
fragment
R                : [Rr] ;
                 
fragment
S                : [Ss] ;
                 
fragment
T                : [Tt] ;
                 
fragment
U                : [Uu] ;
                 
fragment
V                : [Vv] ;
                 
fragment
W                : [Ww] ;
                 
fragment
X                : [Xx] ;
                 
fragment
Y                : [Yy] ;
                 
fragment
Z                : [Zz] ;

fragment
DIGIT            : [0-9] ;

fragment
OCT_DIGIT        : [0-8] ;

fragment
HEX_DIGIT        : [0-9A-Fa-f] ;
                 
fragment
LIST             : L I S T ;
                 
fragment
NAME             : N A M E ;
                 
fragment
BUF_SIZE         :B U F '_' S I Z E ;                 

//----------------------------------------------------------------------------
// Everything from here on (unless we hit another 'mode' command, is related
// to processing IFILEs only. We end the special IFILE mode when we are done
// here.
// It appears that this mode can access DQ_STRING and SQ_STRING in the
// default lexer mode. Interesting. IWS is needed as we are well below the
// default WS rule.
// We need I_WS and I_COMMENT rules here as there is/can be whitespace and
// comments in the tnsnames.ora's IFILE parameters. (I think!)
//----------------------------------------------------------------------------
mode IFILE_MODE ;

I_EQUAL         : '=' ;
I_STRING        : (DQ_STRING | ISQ_STRING | IUQ_STRING) -> popMode ;
ISQ_STRING      : S_QUOTE (~'\'')* S_QUOTE ;
//IUQ_STRING      :  ~["'=\n\r]*? NL ;
IUQ_STRING      :  (~["'=])*? NL ;
I_WS            : [ \t\r\n]+ -> skip  ;
I_COMMENT       : '#' (.)*? NL  -> skip  ;

fragment NL     : '\n' ;

