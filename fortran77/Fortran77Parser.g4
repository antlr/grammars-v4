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

/*
 * Updated by Tom Everett, 2018
 */
parser grammar Fortran77Parser;

options
   { tokenVocab = Fortran77Lexer; }

program
   : executableUnit+ EOL* EOF
   ;

executableUnit
   : functionSubprogram
   | mainProgram
   | subroutineSubprogram
   | blockdataSubprogram
   ;

mainProgram
   : (programStatement)? subprogramBody
   ;

functionSubprogram
   : functionStatement subprogramBody
   ;

subroutineSubprogram
   : subroutineStatement subprogramBody
   ;

blockdataSubprogram
   : blockdataStatement subprogramBody
   ;

otherSpecificationStatement
   : dimensionStatement
   | equivalenceStatement
   | intrinsicStatement
   | saveStatement
   ;

executableStatement
   : (assignmentStatement | gotoStatement | ifStatement | doStatement | continueStatement | stopStatement | pauseStatement | readStatement | writeStatement | printStatement | rewindStatement | backspaceStatement | openStatement | closeStatement | endfileStatement | inquireStatement | callStatement | returnStatement)
   ;

programStatement
   : PROGRAM NAME EOL
   ;

entryStatement
   : ENTRY NAME (LPAREN namelist RPAREN)?
   ;

functionStatement
   : type_? FUNCTION NAME LPAREN namelist? RPAREN EOL?
   ;

blockdataStatement
   : BLOCK NAME
   ;

subroutineStatement
   : SUBROUTINE NAME (LPAREN namelist? RPAREN)? EOL?
   ;

namelist
   : identifier (COMMA identifier)*
   ;

statement
   : entryStatement
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

subprogramBody
   : wholeStatement+ endStatement
   ;

wholeStatement
   : LABEL? statement EOL
   ;
   
endStatement
   : LABEL? END
   ;

dimensionStatement
   : DIMENSION arrayDeclarators
   ;

arrayDeclarator
   : (NAME | REAL) LPAREN arrayDeclaratorExtents RPAREN
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

equivalenceStatement
   : EQUIVALENCE equivEntityGroup (COMMA equivEntityGroup)*
   ;

equivEntityGroup
   : LPAREN equivEntity (COMMA equivEntity)* RPAREN
   ;

equivEntity
   : varRef
   ;

commonStatement
   : COMMON (commonBlock (COMMA commonBlock)* | commonItems)
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

typeStatement
   : typename_ typeStatementNameList
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

typename_
   : (REAL | COMPLEX (STAR ICON?)? | DOUBLE COMPLEX | DOUBLE PRECISION | INTEGER | LOGICAL | CHARACTER)
   ;

type_
   : typename_
   | characterWithLen
   ;

typenameLen
   : STAR ICON
   ;

pointerStatement
   : POINTER pointerDecl (COMMA pointerDecl)*
   ;

pointerDecl
   : LPAREN NAME COMMA NAME RPAREN
   ;

implicitStatement
   : IMPLICIT (implicitNone | implicitSpecs)
   ;

implicitSpec
   : type_ LPAREN implicitLetters RPAREN
   ;

implicitSpecs
   : implicitSpec (COMMA implicitSpec)*
   ;

implicitNone
   : NONE
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

lenSpecification
   : (LPAREN STAR RPAREN) LPAREN STAR RPAREN
   | ICON
   | LPAREN intConstantExpr RPAREN
   ;

characterWithLen
   : characterExpression (cwlLen)?
   ;

cwlLen
   : STAR lenSpecification
   ;

parameterStatement
   : PARAMETER LPAREN paramlist RPAREN
   ;

paramlist
   : paramassign (COMMA paramassign)*
   ;

paramassign
   : NAME ASSIGN constantExpr
   ;

externalStatement
   : EXTERNAL namelist
   ;

intrinsicStatement
   : INTRINSIC namelist
   ;

saveStatement
   : SAVE (saveEntity (COMMA saveEntity)*)?
   ;

saveEntity
   : (NAME | DIV NAME DIV)
   ;

dataStatement
   : DATA dataStatementEntity ((COMMA)? dataStatementEntity)*
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

gotoStatement
   : (GO TO| GOTO) (unconditionalGoto | computedGoto | assignedGoto)
   ;

unconditionalGoto
   : lblRef
   ;

computedGoto
   : LPAREN labelList RPAREN (COMMA)? integerExpr
   ;

lblRef
   : ICON
   ;

labelList
   : lblRef (COMMA lblRef)*
   ;

assignedGoto
   : NAME ((COMMA)? LPAREN labelList RPAREN)?
   ;

ifStatement
   : IF LPAREN logicalExpression RPAREN (blockIfStatement | logicalIfStatement | arithmeticIfStatement)
   ;

arithmeticIfStatement
   : lblRef COMMA lblRef COMMA lblRef
   ;

logicalIfStatement
   : executableStatement
   ;

blockIfStatement
   : firstIfBlock elseIfStatement* elseStatement? endIfStatement
   ;

firstIfBlock
   : THEN EOL? wholeStatement+
   ;

elseIfStatement
   : (ELSEIF | (ELSE IF)) LPAREN logicalExpression RPAREN THEN EOL? wholeStatement+
   ;

elseStatement
   : ELSE EOL? wholeStatement+
   ;

endIfStatement
   : (ENDIF | END IF)
   ;

doStatement
   : DO (doWithLabel | doWithEndDo)
   ;

doVarArgs
   : variableName ASSIGN intRealDpExpr COMMA intRealDpExpr (COMMA intRealDpExpr)?
   ;

doWithLabel
   : lblRef COMMA? doVarArgs EOL? doBody EOL? continueStatement
   ;

doBody
   : (wholeStatement) +
   ;

doWithEndDo
   : doVarArgs EOL? doBody EOL? enddoStatement
   ;

enddoStatement
   : (ENDDO | (END DO))
   ;

continueStatement
   : lblRef? CONTINUE
   ;

stopStatement
   : STOP (ICON | HOLLERITH)?
   ;

pauseStatement
   : PAUSE (ICON | HOLLERITH)
   ;

writeStatement
   : WRITE LPAREN controlInfoList RPAREN ((COMMA? ioList) +)?
   ;

readStatement
   : READ (formatIdentifier ((COMMA ioList) +)?)
   ;

printStatement
   : PRINT (formatIdentifier ((COMMA ioList) +)?)
   ;

assignmentStatement
   : varRef ASSIGN expression
   ;

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

ioList
   : (ioListItem COMMA NAME ASSIGN) ioListItem
   | (ioListItem COMMA ioListItem) ioListItem COMMA ioList
   | ioListItem
   ;

ioListItem
   : (LPAREN ioList COMMA NAME ASSIGN) ioImpliedDoList
   | expression
   ;

ioImpliedDoList
   : LPAREN ioList COMMA NAME ASSIGN intRealDpExpr COMMA intRealDpExpr (COMMA intRealDpExpr)? RPAREN
   ;

openStatement
   : OPEN LPAREN openControl (COMMA openControl)* RPAREN
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
   : FMT
   ;

controlUnit
   : UNIT
   ;

controlRec
   : NAME
   ;

controlEnd
   : END
   ;

controlErr
   : ERR
   ;

controlIostat
   : IOSTART
   ;

controlFile
   : FILE
   ;

controlStatus
   : STATUS
   ;

controlAccess
   : ACCESS
   ;

controlPosition
   : POSITION
   ;

controlForm
   : FORM
   ;

controlRecl
   : RECL
   ;

controlBlank
   : BLANK
   ;

controlExist
   : EXIST
   ;

controlOpened
   : OPENED
   ;

controlNumber
   : NUMBER
   ;

controlNamed
   : NAMED
   ;

controlName
   : NAME
   ;

controlSequential
   : SEQUENTIAL
   ;

controlDirect
   : NAME
   ;

controlFormatted
   : FORMATTED
   ;

controlUnformatted
   : UNFORMATTED
   ;

controlNextrec
   : NEXTREC
   ;

closeStatement
   : CLOSE LPAREN closeControl (COMMA closeControl)* RPAREN
   ;

closeControl
   : unitIdentifier
   | controlUnit ASSIGN unitIdentifier
   | controlErrSpec
   | controlStatus ASSIGN characterExpression
   | controlIostat ASSIGN varRef
   ;

inquireStatement
   : INQUIRE LPAREN inquireControl (COMMA inquireControl)* RPAREN
   ;

inquireControl
   : controlUnit ASSIGN unitIdentifier
   | controlFile ASSIGN characterExpression
   | controlErrSpec
   | (controlIostat | controlExist | controlOpened | controlNumber | controlNamed | controlName | controlAccess | controlSequential | controlDirect | controlForm | controlFormatted | controlUnformatted | controlRecl | controlNextrec | controlBlank) ASSIGN varRef
   | unitIdentifier
   ;

backspaceStatement
   : BACKSPACE berFinish
   ;

endfileStatement
   : ENDFILE berFinish
   ;

rewindStatement
   : REWIND berFinish
   ;

berFinish
   : (unitIdentifier (unitIdentifier) | LPAREN berFinishItem (COMMA berFinishItem)* RPAREN)
   ;

berFinishItem
   : unitIdentifier
   | controlUnit ASSIGN unitIdentifier
   | controlErrSpec
   | controlIostat ASSIGN varRef
   ;

unitIdentifier
   : iexpr
   | STAR
   ;

formatIdentifier
   : (SCON | HOLLERITH)
   | iexpr
   | STAR
   ;

formatStatement
   : FORMAT LPAREN fmtSpec RPAREN
   ;

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

statementFunctionStatement
   : LET sfArgs ASSIGN expression
   ;

sfArgs
   : NAME LPAREN namelist RPAREN
   ;

callStatement
   : CALL subroutineCall
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

returnStatement
   : RETURN (integerExpr)?
   ;

expression
   : ncExpr (COLON ncExpr)?
   ;

ncExpr
   : lexpr0 (concatOp lexpr0)*
   ;

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
   : unsignedArithmeticConstant
   | (HOLLERITH | SCON)
   | logicalConstant
   | varRef
   | LPAREN expression RPAREN
   ;

iexpr
   : iexpr1 ((PLUS | MINUS) iexpr1)*
   ;

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

constantExpr
   : expression
   ;

arithmeticExpression
   : expression
   ;

integerExpr
   : iexpr
   ;

intRealDpExpr
   : expression
   ;

arithmeticConstExpr
   : expression
   ;

intConstantExpr
   : expression
   ;

characterExpression
   : expression
   ;

concatOp
   : DIV DIV
   ;

logicalExpression
   : expression
   ;

logicalConstExpr
   : expression
   ;

arrayElementName
   : NAME LPAREN integerExpr (COMMA integerExpr)* RPAREN
   ;

subscripts
   : LPAREN (expression (COMMA expression)*)? RPAREN
   ;

varRef
   : (NAME | REAL) (subscripts (substringApp)?)?
   ;

varRefCode
   : NAME (subscripts (substringApp)?)?
   ;

substringApp
   : LPAREN (ncExpr)? COLON (ncExpr)? RPAREN
   ;

variableName
   : NAME
   ;

arrayName
   : NAME
   ;

subroutineName
   : NAME
   ;

functionName
   : NAME
   ;

constant
   : ((PLUS | MINUS))? unsignedArithmeticConstant
   | (SCON | HOLLERITH)
   | logicalConstant
   ;

unsignedArithmeticConstant
   : (ICON | RCON)
   | complexConstant
   ;

complexConstant
   : LPAREN ((PLUS | MINUS))? (ICON | RCON) COMMA ((PLUS | MINUS))? (ICON | RCON) RPAREN
   ;

logicalConstant
   : (TRUE | FALSE)
   ;

// needed because Fortran doesn't have reserved keywords. Putting the rule
// 'keyword" instead of a few select keywords breaks the parser with harmful
// non-determinisms
identifier
   : NAME
   | REAL
   ;
