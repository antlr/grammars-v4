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



grammar tnsnames;

// ----------------------------------------------------------------
// Parser rules are lower case, or at least, an initial lower case.
// ----------------------------------------------------------------

//-----------------------------------------------------------------
// Top level rule. Start here with a complete tnsnames.ora file.
//-----------------------------------------------------------------
tnsnames         : (tns_entry | ifile | lsnr_entry)* ;

tns_entry        : alias_list EQUAL (description_list | description) ;

ifile            : IFILE '=' DQ_STRING ;

//-----------------------------------------------------------------
// Listener only entries can be interesting. Here are a couple of
// valid examples, there are others:
//
// LSNR_FRED =
//    (DESCRIPTION=(ADDRESS=(PROTOCOL=tcp)(HOST=xebor04)(PORT=1524)))
//
// LSNR_WILMA =
//    (ADDRESS=(PROTOCOL=IPC)(KEY=LISTENER))
//-----------------------------------------------------------------
lsnr_entry       : alias EQUAL (lsnr_description | address_list | (address)+) ;

lsnr_description : L_PAREN DESCRIPTION EQUAL (address_list | (address)+) R_PAREN ;

//-----------------------------------------------------------------
// Stuff related to alias names. These are weird, they can start
// with a letter or a digit. (See the lexer ID rule below). They
// can also have domains attached - .world, for example. Pretty
// much, anything goes!
//-----------------------------------------------------------------
alias_list       : alias (COMMA alias)* ;

alias            : ID
                 ;

//-----------------------------------------------------------------
// Stuff related to description lists. These seem to be optional in
// the file itself, as you can have multiple descriptions without
// an enclosing description list. And parameters can go almost
// anywhere.
//-----------------------------------------------------------------
description_list : L_PAREN DESCRIPTION_LIST  EQUAL  (dl_params)? (description)+ (dl_params)? R_PAREN ;

dl_params        : dl_parameter+ ;

dl_parameter     : al_failover
                 | al_load_balance
                 | al_source_route
                 ;
                 
//-----------------------------------------------------------------
// Description stuff. Lots of optional parameters scattered willy
// nilly around the description. 
//-----------------------------------------------------------------
description      : L_PAREN DESCRIPTION EQUAL  (d_params)? (address_list | (address)+) (d_params)? connect_data (d_params)? R_PAREN ;

d_params         : d_parameter+ ;

d_parameter      : d_enable
                 | al_failover
                 | al_load_balance
                 | d_sdu
                 | d_recv_buf
                 | d_send_buf
                 | al_source_route                 
                 | d_service_type
                 | d_security
                 | d_conn_timeout
                 | d_retry_count
                 | d_tct
                 ;

d_enable         : L_PAREN ENABLE EQUAL BROKEN R_PAREN ;
                 
d_sdu            : L_PAREN SDU EQUAL INT R_PAREN ;
                 
d_recv_buf       : L_PAREN RECV_BUF EQUAL INT R_PAREN ;
                 
d_send_buf       : L_PAREN SEND_BUF EQUAL INT R_PAREN ;
                 
d_service_type   : L_PAREN SERVICE_TYPE EQUAL ID R_PAREN ;
                 
d_security       : L_PAREN SECURITY EQUAL ds_parameter R_PAREN ;

d_conn_timeout   : L_PAREN CONN_TIMEOUT EQUAL INT R_PAREN ;

d_retry_count    : L_PAREN RETRY_COUNT EQUAL INT R_PAREN ;

d_tct            : L_PAREN TCT EQUAL INT R_PAREN ;
                 
ds_parameter     : L_PAREN SSL_CERT EQUAL DQ_STRING R_PAREN ;                 
                 
//-----------------------------------------------------------------
// Stuff related to address lists. These seem to be optional in
// the file itself, as you can have multiple addresses without
// an enclosing address list. Specific parameters can go almost
// anywhere.
//-----------------------------------------------------------------
address_list     : L_PAREN ADDRESS_LIST EQUAL (al_params)? (address)+ (al_params)? R_PAREN ;

al_params        : al_parameter+ ;

al_parameter     : al_failover              // More to come here ....
                 | al_load_balance
                 | al_source_route
                 ;                  
                 
al_failover      : L_PAREN FAILOVER EQUAL (YES_NO | ON_OFF | TRUE_FALSE) R_PAREN ;

al_load_balance  : L_PAREN LOAD_BALANCE EQUAL (YES_NO | ON_OFF | TRUE_FALSE) R_PAREN ;
                 
al_source_route   : L_PAREN SOURCE_ROUTE EQUAL (YES_NO | ON_OFF) R_PAREN ;
                 
//-----------------------------------------------------------------
// Address stuff. Not much happening here, but the send and receive
// buffer parameters must go at the end, after the protocol stuff.
//-----------------------------------------------------------------
address          : L_PAREN ADDRESS EQUAL  protocol_info (a_params)? R_PAREN ;

a_params         : a_parameter+ ;

a_parameter      : d_send_buf
                 | d_recv_buf
                 ;                 
                 
//-----------------------------------------------------------------
// Protocol stuff next. Currently, only TCP and IPC are defined as
// these are the only ones I use at work. I can test those you see!
//-----------------------------------------------------------------
protocol_info    : tcp_protocol       // More to come here.... BEQ, NMP, SPX ....
                 | ipc_protocol
                 | spx_protocol
                 | nmp_protocol
                 | beq_protocol
                 ;                    // See http://www.toadworld.com/platforms/oracle/w/wiki/5484.defining-tnsname-addresses.aspx
                                      // for examples etc.

//-----------------------------------------------------------------
// TCP Protocol rules.
// (PROTOCOL = TCP)(HOST = hostname)(PORT = portnumber)
//-----------------------------------------------------------------
tcp_protocol     : tcp_params ;
                 
tcp_params       : tcp_parameter+ ;

tcp_parameter    : tcp_host
                 | tcp_port
                 | tcp_tcp
                 ;

tcp_host         : L_PAREN HOST EQUAL host R_PAREN ;
                 
tcp_port         : L_PAREN PORT EQUAL port R_PAREN ;
                 
tcp_tcp          : L_PAREN PROTOCOL EQUAL TCP R_PAREN ;
                 
host             : ID
                 | IP
                 ;

port             : INT ;

//-----------------------------------------------------------------
// IPC Protocol rules.
// (PROTOCOL = IPC)(KEY = something)
//-----------------------------------------------------------------
ipc_protocol     : ipc_params ;
                 
ipc_params       : ipc_parameter+ ;

ipc_parameter    : ipc_ipc
                 | ipc_key
                 ;

ipc_ipc          : L_PAREN PROTOCOL EQUAL IPC R_PAREN ;

ipc_key          : L_PAREN KEY EQUAL ID R_PAREN ;


//-----------------------------------------------------------------
// SPX Protocol rules.
// (PROTOCOL = SPX)(SERVICE = spx_service_name)
//-----------------------------------------------------------------
spx_protocol     : spx_params ;

spx_params       : spx_parameter+ ;

spx_parameter    : spx_spx
                 | spx_service ;

spx_spx          : L_PAREN PROTOCOL EQUAL SPX R_PAREN ;

spx_service      : L_PAREN SERVICE EQUAL ID R_PAREN ;


//-----------------------------------------------------------------
// NMP Protocol rules (Named Pipes).
// (PROTOCOL = NMP)(SERVER = server_name)(PIPE = pipe_name)
//-----------------------------------------------------------------
nmp_protocol     : nmp_params ;

nmp_params       : nmp_parameter+ ;

nmp_parameter    : nmp_nmp
                 | nmp_server
                 | nmp_pipe
                 ;
                 
nmp_nmp          : L_PAREN PROTOCOL EQUAL NMP R_PAREN ;

nmp_server       : L_PAREN SERVER EQUAL ID R_PAREN ;

nmp_pipe         : L_PAREN PIPE EQUAL ID R_PAREN ;
                 

//-----------------------------------------------------------------
// BEQ Protocol rules.
// (PROTOCOL = BEQ)(PROGRAM = oracle_exe)(ARGV0 = sid_identifier)
// (ARGS = '(DESCRIPTION=(LOCAL = YES)(ADDRESS = (PROTOCOL = BEQ)))'
// )                 
//-----------------------------------------------------------------
beq_protocol     : beq_params ;

beq_params       : beq_parameter+ ;

beq_parameter    : beq_beq
                 | beq_program
                 | beq_argv0
                 | beq_args
                 ;
                 
beq_beq          : L_PAREN PROTOCOL EQUAL BEQ R_PAREN ;

beq_program      : L_PAREN PROGRAM EQUAL ID R_PAREN ;

beq_argv0        : L_PAREN ARGV0 EQUAL ID R_PAREN ;

beq_args         : L_PAREN ARGS EQUAL ba_parameter R_PAREN ;

ba_parameter     : S_QUOTE ba_description S_QUOTE ;

ba_description   : L_PAREN DESCRIPTION EQUAL bad_params R_PAREN ;

bad_params       : bad_parameter+ ;

bad_parameter    : bad_local
                 | bad_address
                 ;

bad_local        : L_PAREN LOCAL EQUAL YES_NO R_PAREN ;

bad_address      : L_PAREN ADDRESS EQUAL beq_beq R_PAREN ;


//-----------------------------------------------------------------
// Connect data rules. 
//-----------------------------------------------------------------
connect_data     : L_PAREN CONNECT_DATA EQUAL cd_params R_PAREN ;

cd_params       : cd_parameter+
                ;
                 
cd_parameter     : cd_service_name
                 | cd_sid
                 | cd_instance_name
                 | cd_failover_mode
                 | cd_global_name
                 | cd_hs
                 | cd_rdb_database
                 | cd_server
                 | cd_ur
                 ;

cd_service_name  : L_PAREN SERVICE_NAME EQUAL ID R_PAREN ;

cd_sid           : L_PAREN SID EQUAL ID R_PAREN ;

cd_instance_name : L_PAREN INSTANCE_NAME EQUAL ID R_PAREN ;

cd_failover_mode : L_PAREN FAILOVER_MODE EQUAL fo_params R_PAREN ;

cd_global_name   : L_PAREN GLOBAL_NAME EQUAL ID R_PAREN ;

cd_hs            : L_PAREN HS EQUAL OK R_PAREN ;

// ---------------------------------------------------------------
// This rdb_database one is a tad strange. According to the docs 
// for 11gr2, it can be like (RDB_DATABASE = [.mf]mf_personal.rdb)
// I'm assuming that the [] bit is optional? I have no idea what
// any of this means! ;-)
// ---------------------------------------------------------------
cd_rdb_database  : L_PAREN RDB_DATABASE EQUAL (L_SQUARE DOT ID R_SQUARE)? ID R_PAREN ;

cd_server        : L_PAREN SERVER EQUAL (DEDICATED | SHARED | POOLED) R_PAREN ;
                 
cd_ur            : L_PAREN UR EQUAL UR_A R_PAREN ;                 
                 
fo_params        : fo_parameter+ ;

fo_parameter     : fo_type
                 | fo_backup
                 | fo_method
                 | fo_retries
                 | fo_delay
                 ;
                 
fo_type          : L_PAREN TYPE EQUAL (SESSION | SELECT | NONE) R_PAREN ;

fo_backup        : L_PAREN BACKUP EQUAL ID R_PAREN ;

fo_method        : L_PAREN METHOD EQUAL (BASIC | PRECONNECT) R_PAREN ;

fo_retries       : L_PAREN RETRIES EQUAL INT R_PAREN ;

fo_delay         : L_PAREN DELAY EQUAL INT R_PAREN ;                 
                 


                 
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

IFILE            : I F I L E ; 

                 
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
ID               : [A-Za-z0-9][A-Za-z0-9_\-\.]* ;
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
