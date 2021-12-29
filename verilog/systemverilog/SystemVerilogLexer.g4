// Author: Mustafa Said Ağca
// License: MIT

lexer grammar SystemVerilogLexer;

MINUS: '-';
MINUSMINS: '--';
NOT: '!';
NE: '!=';
NEQ: '!=?';
NEE: '!==';
DPI: '"DPI"';
DPIC: '"DPI-C"';
P: '#';
PP: '##';
PMP: '#-#';
PEP: '#=#';
PZ: '#0';
DOLLAR: '$';
DERROR: '$error';
DFATAL: '$fatal';
DINFO: '$info';
DROOT: '$root.';
DUNIT: '$unit';
DWARNING: '$warning';
PER: '%';
PE: '%=';
AND: '&';
ANDAND: '&&';
ANDANDAND: '&&&';
AE: '&=';
LP: '(';
LPS: '(*';
RP: ')';
STAR: '*';
SRP: '*)';
SS: '**';
SCCS: '*::*';
SEQ: '*=';
SGT: '*>';
COMMA: ',';
DOT: '.';
DOTSTAR: '.*';
SLASH: '/';
SLASHEQ: '/=';
COLON: ':';
MCOLON: '-:';
COLONSLASH: ':/';
COLONCOLON: '::';
COLONEQ: ':=';
SEMI: ';';
QUES: '?';
AT: '@';
ATAT: '@@';
LB: '[';
QUOTE: '\'';
RB: ']';
CARET: '^';
CARETSQUIG: '^~';
CARETEQ: '^=';
LC: '{';
BAR: '|';
BARBAR: '||';
BAREQ: '|=';
BAREQGT: '|=>';
BARARROW: '|->';
RC: '}';
SQUIG: '~';
SQUIGAND: '~&';
SQUIGCARET: '~^';
SQUIGBAR: '~|';
PLUS: '+';
PLUSCOLON: '+:';
PLUSPLUS: '++';
PLUSEQ: '+=';
LT: '<';
LTLT: '<<';
LTLTLT: '<<<';
LTLTLTEQ: '<<<=';
LTLTEQ: '<<=';
LTEQ: '<=';
LTMINUSGT: '<->';
EQ: '=';
MINUSEQ: '-=';
EQEQ: '==';
EQEQQUEST: '==?';
EQEQEQ: '===';
EQGT: '=>';
GT: '>';
ARROW: '->';
GE: '>=';
GTGT: '>>';
MINUSGTGT: '->>';
GTGTEQ: '>>=';
GTGTGT: '>>>';
GTGTGTEQ: '>>>=';
KONESTEP: '1step';
KACCEPT_ON: 'accept_on';
KALIAS: 'alias';
KALWAYS: 'always';
KALWAYS_COMB: 'always_comb';
KALWAYS_FF: 'always_ff';
KALWAYS_LATCH: 'always_latch';
KAND: 'and';
KASSERT: 'assert';
KASSIGN: 'assign';
KASSUME: 'assume';
KAUTOMATIC: 'automatic';
KBEFORE: 'before';
KBEGIN: 'begin';
KBIND: 'bind';
KBINS: 'bins';
KBINSOF: 'binsof';
KBIT: 'bit';
KBREAK: 'break';
KBUF: 'buf';
KBUFIF0: 'bufif0';
KBUFIF1: 'bufif1';
KBYTE: 'byte';
KCASE: 'case';
KCASEX: 'casex';
KCASEZ: 'casez';
KCELL: 'cell';
KCHANDLE: 'chandle';
KCHECKER: 'checker';
KCLASS: 'class';
KCLOCKING: 'clocking';
KCMOS: 'cmos';
KCONFIG: 'config';
KCONST: 'const';
KCONSTRAINT: 'constraint';
KCONTEXT: 'context';
KCONTINUE: 'continue';
KCOVER: 'cover';
KCOVERGROUP: 'covergroup';
KCOVERPOINT: 'coverpoint';
KCROSS: 'cross';
KDEASSIGN: 'deassign';
KDEFAULT: 'default';
KDEFPARAM: 'defparam';
KDESIGN: 'design';
KDISABLE: 'disable';
KDIST: 'dist';
KDO: 'do';
KEDGE: 'edge';
KELSE: 'else';
KEND: 'end';
KENDCASE: 'endcase';
KENDCHECKER: 'endchecker';
KENDCLASS: 'endclass';
KENDCLOCKING: 'endclocking';
KENDCONFIG: 'endconfig';
KENDFUNCTION: 'endfunction';
KENDGENERATE: 'endgenerate';
KENDGROUP: 'endgroup';
KENDINTERFACE: 'endinterface';
KENDMODULE: 'endmodule';
KENDPACKAGE: 'endpackage';
KENDPROGRAM: 'endprogram';
KENDPROPERTY: 'endproperty';
KENDSEQUENCE: 'endsequence';
KENDSPECIFY: 'endspecify';
KENDTASK: 'endtask';
KENUM: 'enum';
KEVENT: 'event';
KEVENTUALLY: 'eventually';
KEXPECT: 'expect';
KEXPORT: 'export';
KEXTENDS: 'extends';
KEXTERN: 'extern';
KFINAL: 'final';
KFIRST_MATCH: 'first_match';
KFOR: 'for';
KFORCE: 'force';
KFOREACH: 'foreach';
KFOREVER: 'forever';
KFORK: 'fork';
KFORKJOIN: 'forkjoin';
KFUNCTION: 'function';
KGENERATE: 'generate';
KGENVAR: 'genvar';
KGLOBAL: 'global';
KHIGHZ0: 'highz0';
KHIGHZ1: 'highz1';
KIF: 'if';
KIFF: 'iff';
KIFNONE: 'ifnone';
KIGNORE_BINS: 'ignore_bins';
KILLEGAL_BINS: 'illegal_bins';
KIMPLEMENTS: 'implements';
KIMPLIES: 'implies';
KIMPORT: 'import';
KINCDIR: '-incdir';
KINCLUDE: 'include';
KINITIAL: 'initial';
KINOUT: 'inout';
KINPUT: 'input';
KINSIDE: 'inside';
KINSTANCE: 'instance';
KINT: 'int';
KINTEGER: 'integer';
KINTERCONNECT: 'interconnect';
KINTERFACE: 'interface';
KINTERSECT: 'intersect';
KJOIN: 'join';
KJOIN_ANY: 'join_any';
KJOIN_NONE: 'join_none';
KLARGE: 'large';
KLET: 'let';
KLIBLIST: 'liblist';
KLIBRARY: 'library';
KLOCAL: 'local';
KLOCALPARAM: 'localparam';
KLOGIC: 'logic';
KLONGINT: 'longint';
KMACROMODULE: 'macromodule';
KMATCHES: 'matches';
KMEDIUM: 'medium';
KMEMBER_IDENTIFIER: 'member_identifier';
KMODPORT: 'modport';
KMODULE: 'module';
KNAND: 'nand';
KNEGEDGE: 'negedge';
KNETTYPE: 'nettype';
KNEW: 'new';
KNEXTTIME: 'nexttime';
KNMOS: 'nmos';
KNOR: 'nor';
KNOSHOWCANCELLED: 'noshowcancelled';
KNOT: 'not';
KNOTIF0: 'notif0';
KNOTIF1: 'notif1';
KNULL: 'null';
KOPTION: 'option';
KOR: 'or';
KOUTPUT: 'output';
KPACKAGE: 'package';
KPACKED: 'packed';
KPARAMETER: 'parameter';
KPATHPULSE: 'PATHPULSE$';
KPMOS: 'pmos';
KPOSEDGE: 'posedge';
KPRIORITY: 'priority';
KPROGRAM: 'program';
KPROPERTY: 'property';
KPROTECTED: 'protected';
KPULL0: 'pull0';
KPULL1: 'pull1';
KPULLDOWN: 'pulldown';
KPULLUP: 'pullup';
KPULSESTYLE_ONDETECT: 'pulsestyle_ondetect';
KPULSESTYLE_ONEVENT: 'pulsestyle_onevent';
KPURE: 'pure';
KRAND: 'rand';
KRANDC: 'randc';
KRANDCASE: 'randcase';
KRANDOMIZE: 'randomize';
KRANDSEQUENCE: 'randsequence';
KRCMOS: 'rcmos';
KREAL: 'real';
KREALTIME: 'realtime';
KREF: 'ref';
KREG: 'reg';
KREJECT_ON: 'reject_on';
KRELEASE: 'release';
KREPEAT: 'repeat';
KRESTRICT: 'restrict';
KRETURN: 'return';
KRNMOS: 'rnmos';
KRPMOS: 'rpmos';
KRTRAN: 'rtran';
KRTRANIF0: 'rtranif0';
KRTRANIF1: 'rtranif1';
KS_ALWAYS: 's_always';
KS_EVENTUALLY: 's_eventually';
KS_NEXTTIME: 's_nexttime';
KS_UNTIL: 's_until';
KS_UNTIL_WITH: 's_until_with';
KSCALARED: 'scalared';
KSEQUENCE: 'sequence';
KSHORTINT: 'shortint';
KSHORTREAL: 'shortreal';
KSHOWCANCELLED: 'showcancelled';
KSIGNED: 'signed';
KSMALL: 'small';
KSOFT: 'soft';
KSOLVE: 'solve';
KSPECIFY: 'specify';
KSPECPARAM: 'specparam';
KSTATIC: 'static';
KSTD: 'std';
KSTRING: 'string';
KSTRONG: 'strong';
KSTRONG0: 'strong0';
KSTRONG1: 'strong1';
KSTRUCT: 'struct';
KSUPER: 'super';
KSUPPLY0: 'supply0';
KSUPPLY1: 'supply1';
KSYNC_ACCEPT_ON: 'sync_accept_on';
KSYNC_REJECT_ON: 'sync_reject_on';
KTAGGED: 'tagged';
KTASK: 'task';
KTHIS: 'this';
KTHROUGHOUT: 'throughout';
KTIME: 'time';
KTIMEPRECISION: 'timeprecision';
KTIMEUNIT: 'timeunit';
KTRAN: 'tran';
KTRANIF0: 'tranif0';
KTRANIF1: 'tranif1';
KTRI: 'tri';
KTRI0: 'tri0';
KTRI1: 'tri1';
KTRIAND: 'triand';
KTRIOR: 'trior';
KTRIREG: 'trireg';
KTYPE: 'type';
KTYPE_OPTION: 'type_option';
KTYPEDEF: 'typedef';
KUNION: 'union';
KUNIQUE: 'unique';
KUNIQUE0: 'unique0';
KUNSIGNED: 'unsigned';
KUNTIL: 'until';
KUNTIL_WITH: 'until_with';
KUNTYPED: 'untyped';
KUSE: 'use';
KUWIRE: 'uwire';
KVAR: 'var';
KVECTORED: 'vectored';
KVIRTUAL: 'virtual';
KVOID: 'void';
KWAIT: 'wait';
KWAIT_ORDER: 'wait_order';
KWAND: 'wand';
KWEAK: 'weak';
KWEAK0: 'weak0';
KWEAK1: 'weak1';
KWHILE: 'while';
KWILDCARD: 'wildcard';
KWIRE: 'wire';
KWITH: 'with';
KWITHIN: 'within';
KWOR: 'wor';
KXNOR: 'xnor';
KXOR: 'xor';

// 22. Compiler directives

COMPILER_DIRECTIVE
	: '`' .*? '\r'? '\n' -> skip
	;

// 33.3.1 Specifying libraries—the library map file

FILE_PATH_SPEC
	: ([/~] | './') ~[ \r\t\n]*?
	;

// A.7.5.3 System timing check event definitions

/*EDGE_DESCRIPTOR
	: '01'
	| '10'
	| Z_OR_X ZERO_OR_ONE
	| ZERO_OR_ONE Z_OR_X
	;

fragment
ZERO_OR_ONE
	: [01]
	;

fragment
Z_OR_X
	: [xXzZ]
	;
*/
// A.8.4 Primaries

TIME_LITERAL
	: UNSIGNED_NUMBER TIME_UNIT
	| FIXED_POINT_NUMBER TIME_UNIT
	;

fragment
TIME_UNIT
	: [mnpf]? 's'
	;

// A.8.7 Numbers

DECIMAL_NUMBER
	: SIZE? DECIMAL_BASE (UNSIGNED_NUMBER | (X_DIGIT | Z_DIGIT) '_'*)
	;

BINARY_NUMBER
	: SIZE? BINARY_BASE BINARY_VALUE
	;

OCTAL_NUMBER
	: SIZE? OCTAL_BASE OCTAL_VALUE
	;

HEX_NUMBER
	: SIZE? HEX_BASE HEX_VALUE
	;

fragment
SIGN
	: [+-]
	;

fragment
SIZE
	: NON_ZERO_UNSIGNED_NUMBER
	;

fragment
NON_ZERO_UNSIGNED_NUMBER
	: NON_ZERO_DECIMAL_DIGIT ('_' | DECIMAL_DIGIT)*
	;

REAL_NUMBER
	: FIXED_POINT_NUMBER
	;

fragment
FIXED_POINT_NUMBER
	: UNSIGNED_NUMBER '.' UNSIGNED_NUMBER
	;

fragment
EXP
	: [eE]
	;

UNSIGNED_NUMBER
	: DECIMAL_DIGIT ('_' | DECIMAL_DIGIT)*
	;

fragment
BINARY_VALUE
	: BINARY_DIGIT ('_' | BINARY_DIGIT)*
	;

fragment
OCTAL_VALUE
	: OCTAL_DIGIT ('_' | OCTAL_DIGIT)*
	;

fragment
HEX_VALUE
	: HEX_DIGIT ('_' | HEX_DIGIT)*
	;

fragment
DECIMAL_BASE
	: '\'' [sS]? [dD]
	;

fragment
BINARY_BASE
	: '\'' [sS]? [bB]
	;

fragment
OCTAL_BASE
	: '\'' [sS]? [oO]
	;

fragment
HEX_BASE
	: '\'' [sS]? [hH]
	;

fragment
NON_ZERO_DECIMAL_DIGIT
	: [1-9]
	;

fragment
DECIMAL_DIGIT
	: [0-9]
	;

fragment
BINARY_DIGIT
	: X_DIGIT
	| Z_DIGIT
	| [01]
	;

fragment
OCTAL_DIGIT
	: X_DIGIT
	| Z_DIGIT
	| [0-7]
	;

fragment
HEX_DIGIT
	: X_DIGIT
	| Z_DIGIT
	| [0-9a-fA-F]
	;

fragment
X_DIGIT
	: [xX]
	;

fragment
Z_DIGIT
	: [zZ?]
	;

/*UNBASED_UNSIZED_LITERAL
	: '\'0'
	| '\'1'
	| '\'' Z_OR_X
	;
*/
// A.8.8 Strings

STRING_LITERAL
	: '"' ~["\n\r]* '"'
	;

// A.9.2 Comments

ONE_LINE_COMMENT
	: '//' .*? '\r'? '\n' -> channel(HIDDEN)
	;

BLOCK_COMMENT
	: '/*' .*? '*/' -> channel(HIDDEN)
	;

// A.9.3 Identifiers

SIMPLE_IDENTIFIER
	: [a-zA-Z_] [a-zA-Z0-9_$]*
	;

SYSTEM_TF_IDENTIFIER
	: '$' [a-zA-Z0-9_$] [a-zA-Z0-9_$]*
	;

// A.9.4 White space

WHITE_SPACE
	: [ \t\n\r]+ -> channel(HIDDEN)
	;
