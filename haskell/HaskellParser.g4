parser grammar HaskellParser;

options { tokenVocab=HaskellLexer; }

module :  semi* pragmas? semi* ((MODULE modid exports? WHERE open body close semi*) | body) EOF;

pragmas
    :
    pragma+
    ;

pragma
    :
    '{-#' 'LANGUAGE'  extension (',' extension)* '#-}'
    ;

extension
    :
    conid
    ;

body
    :
    (impdecls topdecls)
    | impdecls
    | topdecls
    ;

impdecls
    :
    (impdecl | NEWLINE | semi)+
    ;

exports
    :
    '(' (exprt (',' exprt)*)? ','? ')'
    ;

exprt
    :
    qvar
    | ( qtycon ( ('(' '..' ')') | ('(' (cname (',' cname)*)? ')') )? )
    | ( qtycls ( ('(' '..' ')') | ('(' (qvar (',' qvar)*)? ')') )? )
    | ( MODULE modid )
    ;

impdecl
    :
    IMPORT QUALIFIED? modid ('as' modid)? impspec? semi+
    ;

impspec
    :
    ('(' (himport (',' himport)* ','?)? ')')
    | ( 'hiding' '(' (himport (',' himport)* ','?)? ')' )
     ;

himport
    :
    var
    | ( tycon ( ('(' '..' ')') | ('(' (cname (',' cname)*)? ')') )? )
    | ( tycls ( ('(' '..' ')') | ('(' (var (',' var)*)? ')') )? )
    ;

cname
    :
    var | con
    ;

topdecls : ((topdecl semi+) | NEWLINE | semi)+;

topdecl
    :
    (TYPE simpletype '=' type)
    | (DATA (typecontext '=>')? simpletype ('=' constrs)? deriving?)
    | (NEWTYPE (typecontext '=>')? simpletype '=' newconstr deriving?)
    | (CLASS (scontext '=>')? tycls tyvar (WHERE cdecls)?)
    | (INSTANCE (scontext '=>')? qtycls inst (WHERE idecls)?)
    | (DEFAULT '(' (type (',' type)*)? ')' )
    | (FOREIGN fdecl)
    | decl;

decls
    :
    open ((decl semi+)* decl semi*)? close
    ;

decl
    :
    '{-#' 'INLINE' qvar '#-}'
    | '{-#' 'NOINLINE' qvar '#-}'
    | '{-#' 'SPECIALIZE' specs '#-}'
    | gendecl
    | ((funlhs | pat) rhs)
    | semi+
    ;

specs
    :
    spec (',' spec)*
    ;

spec
    :
    vars '::' type
    ;

cdecls
    :
    open ((cdecl semi+)* cdecl semi*)? close
    ;

cdecl
    :
    gendecl
    | ((funlhs | var) rhs)
    ;

idecls
    :
    open ((idecl semi+)* idecl semi*)? close
    ;

idecl
    :
    (funlhs | var) rhs
    ;

gendecl
    :
    vars '::' (typecontext '=>')? type
    | (fixity (DECIMAL)? ops)
    ;

ops
    :
    op (',' op)*
    ;

vars
    :
    var (',' var)*
    ;

fixity
    :
    INFIX | INFIXL | INFIXL
    ;

type
    :
    btype ('->' type)?
    ;

btype
    :
    atype+
    ;

atype
    :
    gtycon
    | varid
    | ( '(' type (',' type)* ')' )
    | ( '[' type ']' )
    | ( '(' type ')' )
    ;

gtycon
    :
    qtycon
    | ( '(' ')' )
    | ( '[' ']' )
    | ( '(' '->' ')' )
    | ( '(' ',' '{' ',' '}' ')' )
    ;

typecontext
    :
    cls
    | ( '(' cls (',' cls)* ')' )
    ;

cls
    :
    (conid varid)
    | ( qtycls '(' tyvar (atype (',' atype)*) ')' )
    ;

scontext
    :
    simpleclass
    | ( '(' (simpleclass (',' simpleclass)*)? ')' )
    ;

simpleclass
    :
    qtycls tyvar
    ;

simpletype
    :
    tycon tyvar*
    ;

constrs
    :
    constr ('|' constr)*
    ;

constr
    :
    (con ('!'? atype)*)
    | ((btype | ('!' atype)) conop (btype | ('!' atype)))
    | (con '{' (fielddecl (',' fielddecl)* )? '}')
    ;

newconstr
    :
    (con atype)
    | (con '{' var '::' type '}')
    ;

fielddecl
    :
    vars '::' (type | ('!' atype))
    ;

deriving
    :
    DERIVING (dclass | ('(' (dclass (',' dclass)*)? ')' ))
    ;

dclass
    :
    qtycls
    ;

inst
    :
    gtycon
    | ( '(' gtycon tyvar* ')' )
    | ( '(' gtycon tycon* ')' )
    | ( '(' tyvar ',' tyvar (',' tyvar)* ')')
    | ( '[' tyvar ']')
    | ( '(' tyvar '->' tyvar ')' )
    ;

fdecl
    :
    (IMPORT callconv safety? impent var '::' type)
    | (EXPORT callconv expent var '::' type)
    ;

callconv
    :
    'ccall' | 'stdcall' | 'cplusplus' | 'jvm' | 'dotnet'
    ;

impent : pstring;
expent : pstring;
safety : 'unsafe' | 'safe';

funlhs
    :
    (var apat+)
    | (pat varop pat)
    | ( '(' funlhs ')' apat+)
    ;

rhs
    :
    ('=' exp (WHERE decls)?)
    | (gdrhs (WHERE decls)?);

gdrhs
    :
    gdrh+
    ;

gdrh
    :
    '|' guards '=' exp
    ;

guards
    :
    guard (',' guard)*
    ;

guard
    :
    pat '<-' infixexp
    | LET decls
    | infixexp
    ;

exp
    :
    (infixexp '::' (typecontext '=>')? type)
    | infixexp
    ;

infixexp
    :
    (lexp qop infixexp)
    | ('-' infixexp)
    | lexp
    ;

lexp
    :
    ('\\' apat+ '->' exp)
    | (LET decls IN exp)
    | (IF exp semi? THEN exp semi? ELSE exp)
    | (IF ifgdpats)
    | (CASE exp OF alts)
    | (DO stmts)
    | fexp
    ;

fexp
    :
    aexp+
    ;

aexp
    :
    qvar
    | gcon
    | literal
    | ( '(' exp ')' )
    | ( '(' exp ',' exp (',' exp)* ')' )
    | ( '[' exp (',' exp)* ']' )
    | ( '[' exp (',' exp)? '..' exp? ']' )
    | ( '[' exp '|' qual (',' qual)* ']' )
    | ( '(' infixexp qop ')' )
    | ( '(' qop infixexp ')' )
    | ( qcon '{' (fbind (',' fbind))? '}' )
    | ('{' fbind (',' fbind)* '}')+
    ;

qual
    :
    (pat '<-' exp)
    | (LET decls)
    | exp
    ;

alts
    :
    open (alt semi+)+ close
    ;

alt
    :
    (pat '->' exp (WHERE decls)?)
    | (pat gdpats (WHERE decls)?)
    ;

gdpats
    :
    gdpat+
    ;

// In ghc parser on GitLab second rule is 'gdpats close'
// Unclearly possible errors with this implemmentation

// Now extension is always works (follow semantic predicate in 'lexp' rule)
ifgdpats
    :
    '{' gdpats '}'
    | gdpats
    ;

gdpat
    :
    '|' guards '->' exp
    ;

stmts
    :
    open (stmt)* exp semi* close
    ;

stmt
    :
    (exp semi+)
    | (pat '<-' exp semi+)
    | (LET decls semi+)
    | semi+
    ;

fbind
    :
    qvar '=' exp
    ;

pat
    :
    (lpat qconop pat)
    | lpat
    ;

lpat
    :
    apat
    | ('-' (integer | pfloat))
    | (gcon apat+)
    ;

apat
    :
    (var ('@' apat)?)
    | gcon
    | (qcon '{' (fpat (',' fpat)*)? '}')
    | literal
    | '_'
    | ('(' pat ')')
    | ('(' pat ',' pat (',' pat)* ')')
    | ('[' pat (',' pat)* ']')
    | ('~'apat)
    ;

fpat
    :
    qvar '=' pat
    ;

gcon
    :
    ('(' ')')
    | ('[' ']')
    | ('(' (',')+ ')')
    | qcon
    ;

var	:    varid   | ( '(' varsym ')' );
qvar:    qvarid  | ( '(' qvarsym ')');
con :    conid   | ( '(' consym ')' );
qcon:    qconid  | ( '(' gconsym ')');
varop:   varsym  | ('`' varid '`')   ;
qvarop:  qvarsym | ('`' qvarid '`')	 ;
conop:   consym  | ('`' conid '`')	 ;
qconop:  gconsym | ('`' qconid '`')	 ;
op:      varop   | conop			 ;
qop:     qvarop  | qconop			 ;
gconsym: ':'  	 | qconsym			 ;

open : VOCURLY | OCURLY;
close : VCCURLY | CCURLY;
semi : ';' | SEMI;

literal : integer | pfloat | pchar | pstring;
special : '(' | ')' | ',' | ';' | '[' | ']' | '`' | '{' | '}';

varid : (VARID | AS | HIDING) '#'*;
conid : CONID '#'*;

symbol: ascSymbol;
ascSymbol: '!' | '#' | '$' | '%' | '&' | '*' | '+'
        | '.' | '/' | '<' | '=' | '>' | '?' | '@'
        | '\\' | '^' | '|' | '-' | '~' | ':' ;

varsym : ascSymbol+;
consym : ':' ascSymbol*;

tyvar : varid;
tycon : conid;
tycls : conid;
modid : (conid '.')* conid;

qvarid : (modid '.')? varid;
qconid : (modid '.')? conid;
qtycon : (modid '.')? tycon;
qtycls : (modid '.')? tycls;
qvarsym: (modid '.')? varsym;
qconsym: (modid '.')? consym;

integer
    :
    DECIMAL
    | OCTAL
    | HEXADECIMAL
    ;


pfloat: FLOAT;
pchar: CHAR;
pstring: STRING;
