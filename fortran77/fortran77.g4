/*
 * Fortran 77 grammar for ANTLR 2.7.5
 * Adadpted from Fortran 77 PCCTS grammar by Olivier Dragon
 * Original PCCTS grammar by Terence Parr
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
 *
 */

/**
 * ported to Antlr4 by Tom Everett
 */
grammar fortran77;

/* Start rule */
program
   : ((~ COMMENT) executableUnit | (: COMMENT) +) +
   ;

/* one unit of a fortran program */
executableUnit
   : (functionStatement) functionSubprogram
   | mainProgram
   | subroutineSubprogram
   | blockdataSubprogram
   ;

/* 2 */
mainProgram
   : (programStatement)? subprogramBody
   ;

/* 3 */
functionSubprogram
   : functionStatement subprogramBody
   ;

/* 4 */
subroutineSubprogram
   : subroutineStatement subprogramBody
   ;

/* 5 - blockDataSubprogram */
blockdataSubprogram
   : blockdataStatement subprogramBody
   ;

/* 6 */
otherSpecificationStatement
   : dimensionStatement
   | equivalenceStatement
   | intrinsicStatement
   | saveStatement
   ;

/* 7 */
executableStatement
   : (assignmentStatement | gotoStatement | ifStatement | doStatement | continueStatement | stopStatement | pauseStatement | readStatement | writeStatement | printStatement | rewindStatement | backspaceStatement | openStatement | closeStatement | endfileStatement | inquireStatement | callStatement | returnStatement)
   ;

/* 8 */
programStatement
   : 'program' NAME seos
   ;

seos
   : EOS
   ;

/* 9, 11, 13 */
entryStatement
   : 'entry' NAME (LPAREN namelist RPAREN)?
   ;

/* 10 */
functionStatement
   : (type)? 'function' NAME LPAREN (namelist)? RPAREN seos
   ;

blockdataStatement
   : 'block' NAME seos
   ;

/* 12 */
subroutineStatement
   : 'subroutine' NAME (LPAREN (namelist)? RPAREN)? seos
   ;

namelist
   : identifier (COMMA identifier)*
   ;

statement
   : formatStatement
   | entryStatement
   | implicitStatement
   | parameterStatement
   | typeStatement
   | commonStatement
   | pointerStatement
   | externalStatement
   | otherSpecificationStatement
   | dataStatement
   | (statementFunctionStatement) statementFunctionStatement
   | executableStatement
   ;

/* 2,3,4,5 body of a subprogram (after program/subroutine/function line) */
subprogramBody
   : (wholeStatement)* endStatement
   ;

wholeStatement
   : COMMENT
   | (LABEL)? statement seos
   ;

endStatement
   : (LABEL)? 'end' seos
   ;

/* 15 */
dimensionStatement
   : 'dimension' arrayDeclarators
   ;

/* 16 */
arrayDeclarator
   : (NAME | 'real') LPAREN arrayDeclaratorExtents RPAREN
   ;

arrayDeclarators
   : arrayDeclarator (COMMA arrayDeclarator)*
   ;

arrayDeclaratorExtents
   : arrayDeclaratorExtent (COMMA arrayDeclaratorExtent)*
   ;

arrayDeclaratorExtent
   : iexprCode (COLON (iexprCode | STAR))?
   | STAR
   ;

/* 17 */
equivalenceStatement
   : 'equivalence' equivEntityGroup (COMMA equivEntityGroup)*
   ;

equivEntityGroup
   : LPAREN equivEntity (COMMA equivEntity)* RPAREN
   ;

/* 18 */
equivEntity
   : varRef
   ;

/* 19 */
commonStatement
   : 'common' (commonBlock (COMMA commonBlock)* | commonItems)
   ;

commonName
   : DIV (NAME DIV | DIV)
   ;

commonItem
   : NAME
   | arrayDeclarator
   ;

commonItems
   : commonItem (COMMA commonItem)*
   ;

commonBlock
   : commonName commonItems
   ;

/* 20 */
// need to expand the typename rule to produce a better AST
typeStatement
   : typename typeStatementNameList
   | characterWithLen typeStatementNameCharList
   ;

typeStatementNameList
   : typeStatementName (COMMA typeStatementName)*
   ;

typeStatementName
   : NAME
   | arrayDeclarator
   ;

typeStatementNameCharList
   : typeStatementNameChar (COMMA typeStatementNameChar)*
   ;

typeStatementNameChar
   : typeStatementName (typeStatementLenSpec)?
   ;

typeStatementLenSpec
   : STAR lenSpecification
   ;

typename
   : ('real' | 'complex' (STAR ICON?)? | 'double' 'complex' | 'double' 'precision' | 'integer' | 'logical')
   ;

type
   : typename
   | characterWithLen
   ;

typenameLen
   : STAR ICON
   ;

/* 'Cray' pointer */
pointerStatement
   : 'pointer' pointerDecl (COMMA pointerDecl)*
   ;

pointerDecl
   : LPAREN NAME COMMA NAME RPAREN
   ;

/* 21 */
implicitStatement
   : 'implicit' (implicitNone | implicitSpecs)
   ;

implicitSpec
   : type LPAREN implicitLetters RPAREN
   ;

implicitSpecs
   : implicitSpec (COMMA implicitSpec)*
   ;

implicitNone
   : 'none'
   ;

implicitLetter
   : NAME
   ;

implicitRange
   : implicitLetter (MINUS implicitLetter)?
   ;

implicitLetters
   : implicitRange (COMMA implicitRange)*
   ;

/* 22 */
lenSpecification
   : (LPAREN STAR RPAREN) LPAREN STAR RPAREN
   | ICON
   | LPAREN intConstantExpr RPAREN
   ;

characterWithLen
   : 'character' (cwlLen)?
   ;

cwlLen
   : STAR lenSpecification
   ;

/* 23 */
parameterStatement
   : 'parameter' LPAREN paramlist RPAREN
   ;

paramlist
   : paramassign (COMMA paramassign)*
   ;

paramassign
   : NAME ASSIGN constantExpr
   ;

/* 24 */
externalStatement
   : 'external' namelist
   ;

/* 25 */
intrinsicStatement
   : 'intrinsic' namelist
   ;

/* 26 */
saveStatement
   : 'save' (saveEntity (COMMA saveEntity)*)?
   ;

saveEntity
   : (NAME | DIV NAME DIV)
   ;

/* 27 */
dataStatement
   : 'data' dataStatementEntity ((COMMA)? dataStatementEntity)*
   ;

dataStatementItem
   : varRef
   | dataImpliedDo
   ;

dataStatementMultiple
   : ((ICON | NAME) STAR)? (constant | NAME)
   ;

dataStatementEntity
   : dse1 dse2
   ;

dse1
   : dataStatementItem (COMMA dataStatementItem)* DIV
   ;

dse2
   : dataStatementMultiple (COMMA dataStatementMultiple)* DIV
   ;

/* 28 */
dataImpliedDo
   : LPAREN dataImpliedDoList COMMA dataImpliedDoRange RPAREN
   ;

dataImpliedDoRange
   : NAME ASSIGN intConstantExpr COMMA intConstantExpr (COMMA intConstantExpr)?
   ;

dataImpliedDoList
   : dataImpliedDoListWhat
   | COMMA dataImpliedDoList
   ;

dataImpliedDoListWhat
   : (varRef | dataImpliedDo)
   ;

/* 29 */
assignmentStatement
   : varRef ASSIGN expression
   |
   //	'let' varRef ASSIGN expression | 'assign' ICON to variableName
   ;

/* 30 */
gotoStatement
   : ('goto' | 'go' to) (unconditionalGoto | computedGoto | assignedGoto)
   ;

/* 31 */
unconditionalGoto
   : lblRef
   ;

/* 32 */
computedGoto
   : LPAREN labelList RPAREN (COMMA)? integerExpr
   ;

lblRef
   : ICON
   ;

labelList
   : lblRef (COMMA lblRef)*
   ;

/* 33 */
assignedGoto
   : NAME ((COMMA)? LPAREN labelList RPAREN)?
   ;

/* 34 */
ifStatement
   : 'if' LPAREN logicalExpression RPAREN (blockIfStatement | logicalIfStatement | arithmeticIfStatement)
   ;

arithmeticIfStatement
   : lblRef COMMA lblRef COMMA lblRef
   ;

/* 35 */
logicalIfStatement
   : executableStatement
   ;

/* 36 */
blockIfStatement
   : firstIfBlock (: elseIfStatement)* (elseStatement)? endIfStatement
   ;

firstIfBlock
   : 'then' seos (wholeStatement) +
   ;

/* 37 */
elseIfStatement
   : ('elseif' | 'else' 'if') LPAREN logicalExpression RPAREN 'then' seos (wholeStatement)*
   ;

/* 38 */
elseStatement
   : 'else' seos (wholeStatement)*
   ;

/* 39 */
endIfStatement
   : ('endif' | 'end' 'if')
   ;

/* 40 */
doStatement
   : 'do' (doWithLabel | doWithEndDo)
   ;

doVarArgs
   : variableName ASSIGN intRealDpExpr COMMA intRealDpExpr (COMMA intRealDpExpr)?
   ;

doWithLabel
   : lblRef (COMMA)? doVarArgs
   ;

doBody
   : (wholeStatement)*
   ;

doWithEndDo
   : doVarArgs doBody enddoStatement
   ;

enddoStatement
   : ('enddo' | 'end' 'do')
   ;

/* 41 */
continueStatement
   : 'continue'
   ;

/* 42 */
stopStatement
   : 'stop' (ICON | HOLLERITH)?
   ;

/* 43 */
pauseStatement
   : 'pause' (ICON | HOLLERITH)
   ;

/* 44 */
writeStatement
   : 'write' LPAREN controlInfoList RPAREN (ioList)?
   ;

/* 45 */
readStatement
   : 'read' ((formatIdentifier (COMMA ioList)? EOS) (formatIdentifier (COMMA ioList)?) | LPAREN controlInfoList RPAREN (ioList)?)
   ;

/* 46 */
printStatement
   : 'print' formatIdentifier (COMMA ioList)?
   ;

/* 47 */
controlInfoList
   : controlInfoListItem (COMMA controlInfoListItem)*
   ;

controlErrSpec
   : controlErr ASSIGN (lblRef | NAME)
   ;

controlInfoListItem
   : unitIdentifier
   | (HOLLERITH | SCON)
   | controlFmt ASSIGN formatIdentifier
   | controlUnit ASSIGN unitIdentifier
   | controlRec ASSIGN integerExpr
   | controlEnd ASSIGN lblRef
   | controlErrSpec
   | controlIostat ASSIGN varRef
   ;

/* 48 */
/* ioList : (ioListItem COMMA ioList) | ioListItem ; */
ioList
   : (ioListItem COMMA NAME ASSIGN) ioListItem
   | (ioListItem COMMA ioListItem) ioListItem COMMA ioList
   | ioListItem
   ;

ioListItem
   : (LPAREN ioList COMMA NAME ASSIGN) ioImpliedDoList
   | expression
   ;

/* 49 */
ioImpliedDoList
   : LPAREN ioList COMMA NAME ASSIGN intRealDpExpr COMMA intRealDpExpr (COMMA intRealDpExpr)? RPAREN
   ;

/* 50 */
openStatement
   : 'open' LPAREN openControl (COMMA openControl)* RPAREN
   ;

openControl
   : unitIdentifier
   | controlUnit ASSIGN unitIdentifier
   | controlErrSpec
   | controlFile ASSIGN characterExpression
   | controlStatus ASSIGN characterExpression
   | (controlAccess | controlPosition) ASSIGN characterExpression
   | controlForm ASSIGN characterExpression
   | controlRecl ASSIGN integerExpr
   | controlBlank ASSIGN characterExpression
   | controlIostat ASSIGN varRef
   ;

controlFmt
   : 'fmt'
   ;

controlUnit
   : 'unit'
   ;

controlRec
   : NAME
   ;

controlEnd
   : 'end'
   ;

controlErr
   : 'err'
   ;

controlIostat
   : 'iostat'
   ;

controlFile
   : 'file'
   ;

controlStatus
   : 'status'
   ;

controlAccess
   : 'access'
   ;

controlPosition
   : 'position'
   ;

controlForm
   : 'form'
   ;

controlRecl
   : 'recl'
   ;

controlBlank
   : 'blank'
   ;

controlExist
   : 'exist'
   ;

controlOpened
   : 'opened'
   ;

controlNumber
   : 'number'
   ;

controlNamed
   : 'named'
   ;

controlName
   : 'name'
   ;

controlSequential
   : 'sequential'
   ;

controlDirect
   : NAME
   ;

controlFormatted
   : 'formatted'
   ;

controlUnformatted
   : 'unformatted'
   ;

controlNextrec
   : 'nextrec'
   ;

/* 51 */
closeStatement
   : 'close' LPAREN closeControl (COMMA closeControl)* RPAREN
   ;

closeControl
   : unitIdentifier
   | controlUnit ASSIGN unitIdentifier
   | controlErrSpec
   | controlStatus ASSIGN characterExpression
   | controlIostat ASSIGN varRef
   ;

/* 52 */
inquireStatement
   : 'inquire' LPAREN inquireControl (COMMA inquireControl)* RPAREN
   ;

inquireControl
   : controlUnit ASSIGN unitIdentifier
   | controlFile ASSIGN characterExpression
   | controlErrSpec
   | (controlIostat | controlExist | controlOpened | controlNumber | controlNamed | controlName | controlAccess | controlSequential | controlDirect | controlForm | controlFormatted | controlUnformatted | controlRecl | controlNextrec | controlBlank) ASSIGN varRef
   | unitIdentifier
   ;

/* 53 */
backspaceStatement
   : 'backspace' berFinish
   ;

/* 54 */
endfileStatement
   : 'endfile' berFinish
   ;

/* 55 */
rewindStatement
   : 'rewind' berFinish
   ;

berFinish
   : ((unitIdentifier EOS) (unitIdentifier) | LPAREN berFinishItem (COMMA berFinishItem)* RPAREN)
   ;

berFinishItem
   : unitIdentifier
   | controlUnit ASSIGN unitIdentifier
   | controlErrSpec
   | controlIostat ASSIGN varRef
   ;

/* 56 */
unitIdentifier
   : iexpr
   | STAR
   ;

/* 57 */
formatIdentifier
   : (SCON | HOLLERITH)
   | iexpr
   | STAR
   ;

/* 58-59 */
formatStatement
   : 'format' LPAREN fmtSpec RPAREN
   ;

/* 60 */
fmtSpec
   : (formatedit | formatsep (formatedit)?) (formatsep (formatedit)? | COMMA (formatedit | formatsep (formatedit)?))*
   ;

formatsep
   : DIV
   | COLON
   | DOLLAR
   ;

formatedit
   : XCON
   | editElement
   | ICON editElement
   | (PLUS | MINUS)? PCON ((ICON)? editElement)?
   ;

editElement
   : (FCON | SCON | HOLLERITH | NAME)
   | LPAREN fmtSpec RPAREN
   ;

/* 70 */
statementFunctionStatement
   : 'let' sfArgs ASSIGN expression
   ;

sfArgs
   : NAME LPAREN namelist RPAREN
   ;

/* 71 */
callStatement
   : 'call' subroutineCall
   ;

subroutineCall
   : NAME (LPAREN (callArgumentList)? RPAREN)?
   ;

callArgumentList
   : callArgument (COMMA callArgument)*
   ;

callArgument
   : expression
   | STAR lblRef
   ;

/* 72 */
returnStatement
   : 'return' (integerExpr)?
   ;

/* 74 */
expression
   : ncExpr (COLON ncExpr)?
   ;

ncExpr
   : lexpr0 (concatOp lexpr0)*
   ;

// concatenation
lexpr0
   : lexpr1 ((NEQV | EQV) lexpr1)*
   ;

lexpr1
   : lexpr2 (LOR lexpr2)*
   ;

lexpr2
   : lexpr3 (LAND lexpr3)*
   ;

lexpr3
   : LNOT lexpr3
   | lexpr4
   ;

lexpr4
   : aexpr0 ((LT | LE | EQ | NE | GT | GE) aexpr0)?
   ;

aexpr0
   : aexpr1 ((PLUS | MINUS) aexpr1)*
   ;

aexpr1
   : aexpr2 ((STAR | DIV) aexpr2)*
   ;

aexpr2
   : (PLUS | MINUS)* aexpr3
   ;

aexpr3
   : aexpr4 (POWER aexpr4)*
   ;

aexpr4
   : (unsignedArithmeticConstant) unsignedArithmeticConstant
   | (HOLLERITH | SCON)
   | logicalConstant
   | varRef
   | LPAREN expression RPAREN
   ;

/* integer expression */
iexpr
   : iexpr1 ((PLUS | MINUS) iexpr1)*
   ;

/* integer expression with fpe return code. */
iexprCode
   : iexpr1 ((PLUS | MINUS) iexpr1)*
   ;

iexpr1
   : iexpr2 ((STAR | DIV) iexpr2)*
   ;

iexpr2
   : (PLUS | MINUS)* iexpr3
   ;

iexpr3
   : iexpr4 (POWER iexpr3)?
   ;

iexpr4
   : ICON
   | varRefCode
   | LPAREN iexprCode RPAREN
   ;

/* 75 */
constantExpr
   : expression
   ;

/* 76 */
arithmeticExpression
   : expression
   ;

/* 77 */
integerExpr
   : iexpr
   ;

/* 78 */
intRealDpExpr
   : expression
   ;

/* 79 */
arithmeticConstExpr
   : expression
   ;

/* 80 */
intConstantExpr
   : expression
   ;

/* 82 */
characterExpression
   : expression
   ;

concatOp
   : DIV DIV
   ;

/* 84 */
logicalExpression
   : expression
   ;

/* 85 */
logicalConstExpr
   : expression
   ;

/* 88 */
arrayElementName
   : NAME LPAREN integerExpr (COMMA integerExpr)* RPAREN
   ;

subscripts
   : LPAREN (expression (COMMA expression)*)? RPAREN
   ;

varRef
   : (NAME | 'real') (subscripts (substringApp)?)?
   ;

varRefCode
   : NAME (subscripts (substringApp)?)?
   ;

substringApp
   : LPAREN (ncExpr)? COLON (ncExpr)? RPAREN
   ;

/* 91 */
variableName
   : NAME
   ;

/* 92 */
arrayName
   : NAME
   ;

/* 97 */
subroutineName
   : NAME
   ;

/* 98 */
functionName
   : NAME
   ;

/* 100 */
constant
   : ((PLUS | MINUS))? unsignedArithmeticConstant
   | (SCON | HOLLERITH)
   | logicalConstant
   ;

/* 101 */
unsignedArithmeticConstant
   : (ICON | RCON)
   | complexConstant
   ;

/* 107 */
complexConstant
   : LPAREN ((PLUS | MINUS))? (ICON | RCON) COMMA ((PLUS | MINUS))? (ICON | RCON) RPAREN
   ;

/* 108 */
logicalConstant
   : (TRUE | FALSE)
   ;

// needed because Fortran doesn't have reserved keywords. Putting the rule
// 'keyword" instead of a few select keywords breaks the parser with harmful
// non-determinisms
identifier
   : NAME
   | ('real')
   ;

to
   : NAME
   ;

// keyword contains all of the FORTRAN keywords.
keyword
   : 'program'
   | 'entry'
   | 'function'
   | 'block'
   | 'subroutine'
   | 'end'
   | 'dimension'
   | 'equivalence'
   | 'common'
   | 'real' 'complex'
   | 'double'
   | 'precision'
   | 'integer'
   | 'logical'
   | 'pointer'
   | 'implicit'
   | 'none'
   | 'character'
   | 'parameter'
   | 'external'
   | 'intrinsic'
   | 'save'
   | 'data'
   | 'assign'
   |
   //    'to' | 'goto'
   | 'go'
   | 'if'
   | 'then'
   | 'elseif'
   | 'else'
   | 'endif'
   | 'do'
   | 'enddo'
   | 'continue'
   | 'stop'
   | 'pause'
   | 'write'
   | 'read'
   | 'print'
   | 'open'
   | 'fmt'
   | 'unit'
   |
   //    'rec' | 'err'
   | 'iostat'
   | 'file'
   | 'status'
   | 'access'
   | 'position'
   | 'form'
   | 'recl'
   | 'blank'
   | 'exist'
   | 'opened'
   | 'number'
   | 'named'
   | 'name'
   | 'sequential'
   |
   //    'direct' | 'formatted'
   | 'unformatted'
   | 'nextrec'
   | 'close'
   | 'inquire'
   | 'backspace'
   | 'endfile'
   | 'rewind'
   | 'format'
   | 'let'
   | 'call'
   | 'return'
   ;

// Need 4 lookahead for logical operators (eg .NE. and .NEQV.)

DOLLAR
   : '$'
   ;


COMMA
   : ','
   ;


LPAREN
   : '('
   ;


RPAREN
   : ')'
   ;


COLON
   : ':'
   ;

//CONCAT     : '//' ; // define in parser. Not all // are concat ops.

ASSIGN
   : '='
   ;


MINUS
   : '-'
   ;


PLUS
   : '+'
   ;


DIV
   : '/'
   ;


STAR
   : '*'
   ;

// not a comment

POWER
   : '**'
   ;

// not a comment

LNOT
   : '.not.'
   ;


LAND
   : '.and.'
   ;


LOR
   : '.or.'
   ;


EQV
   : '.eqv.'
   ;


NEQV
   : '.neqv.'
   ;


XOR
   : '.xor.'
   ;


EOR
   : '.eor.'
   ;


LT
   : '.lt.'
   ;


LE
   : '.le.'
   ;


GT
   : '.gt.'
   ;


GE
   : '.ge.'
   ;


NE
   : '.ne.'
   ;


EQ
   : '.eq.'
   ;


TRUE
   : '.true.'
   ;


FALSE
   : '.false.'
   ;

//LABELREF:	LABELREF; // reference to a defined LABEL, eg. in a goto statement

XCON
   : 'XCON'
   ;


PCON
   : 'PCON'
   ;


FCON
   : 'FCON'
   ;


RCON
   : 'RCON'
   ;


CCON
   : 'CCON'
   ;


HOLLERITH
   : 'HOLLERITH'
   ;


CONCATOP
   : 'CONCATOP'
   ;


CTRLDIRECT
   : 'CTRLDIRECT'
   ;


CTRLREC
   : 'CTRLREC'
   ;


TO
   : 'TO'
   ;


SUBPROGRAMBLOCK
   : 'SUBPROGRAMBLOCK'
   ;


DOBLOCK
   : 'DOBLOCK'
   ;


AIF
   : 'AIF'
   ;


THENBLOCK
   : 'THENBLOCK'
   ;


ELSEIF
   : 'ELSEIF'
   ;


ELSEBLOCK
   : 'ELSEBLOCK'
   ;


CODEROOT
   : 'CODEROOT'
   ;


CONTINUATION
   : ~ ('0' | ' ')
   ;


EOS
   : (('\n' | '\r' ('\n')?)) + (('     ' CONTINUATION) '     ' CONTINUATION |)
   ;

// Fortran 77 doesn't allow for empty lines. Therefore EOS (newline) is NOT
// part of ignored white spaces. It is only ignored for line continuations.

WS
   : WHITE -> skip
   ;

// Fortran 77 comments must start with the character on the first column
// we keep the comments inside the AST. See parser rules "wholeStatement".
// We however trim empty comment lines.

COMMENT
   : ('c' | '*') (('%' '&' (NOTNL)* |) | (NOTNL) +) (('\n' | '\r' ('\n')?)) +
   ;

// '' is used to drop the charater when forming the lexical token
// Strings are assumed to start with a single quote (') and two
// single quotes is meant as a literal single quote

SCON
   : '\'' ('\'' '\'' | ~ ('\'' | '\n' | '\r') | (('\n' | '\r' ('\n')?) '     ' CONTINUATION) ('\n' | '\r' ('\n')?) '     ' CONTINUATION)* '\''
   ;

// numeral literal

ZCON
   : 'z' '\'' (HEX) + '\''
   ;

// identifier (keyword or variable)

NAME
   : (('i' | 'f' | 'd' | 'g' | 'e') (NUM) + '.') FDESC | ALPHA (ALNUM)*
   ;


WHITE
   : (' ' | '\t')
   ;


ALPHA
   : ('a' .. 'z')
   ;

// case-insensitive

NUM
   : ('0' .. '9')
   ;


ALNUM
   : (ALPHA | NUM)
   ;


HEX
   : (NUM | 'a' .. 'f')
   ;


SIGN
   : ('+' | '-')
   ;


NOTNL
   : ~ ('\n' | '\r')
   ;


INTVAL
   : (NUM) +
   ;


FDESC
   : ('i' | 'f' | 'd') (NUM) + '.' (NUM) + | ('e' | 'g') (NUM) + '.' (NUM) + ('e' (NUM) +)?
   ;


EXPON
   : ('e' | 'd') (SIGN)? (NUM) +
   ;
