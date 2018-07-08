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
   : executableUnit +
   ;

/* one unit of a fortran program */
executableUnit
   : functionSubprogram
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
   : (assignmentStatement | gotoStatement | ifStatement | doStatement | continueStatement | stopStatement | pauseStatement | readStatement | writeStatement | printStatement | rewindStatement | backspaceStatement | openStatement | closeStatement | endfileStatement | inquireStatement | callStatement | returnStatement){};
   
   /* 8 */
   programStatement
      : 'program' NAME seos
      | 'PROGRAM' NAME seos
      ;
   
   seos
      : ('\r\n' | '\n')
      ;
   
   /* 9, 11, 13 */
   entryStatement
      : 'entry' NAME (LPAREN namelist RPAREN)?
      | 'ENTRY' NAME (LPAREN namelist RPAREN)?
      ;
   
   /* 10 */
   functionStatement
      : (type)? 'function' NAME LPAREN (namelist)? RPAREN seos
      | (type)? 'FUNCTION' NAME LPAREN (namelist)? RPAREN seos
      ;
   
   blockdataStatement
      : 'block' NAME seos
      | 'BLOCK' NAME seos
      ;
   
   /* 12 */
   subroutineStatement
      : 'subroutine' NAME (LPAREN (namelist)? RPAREN)? seos
      | 'SUBROUTINE' NAME (LPAREN (namelist)? RPAREN)? seos
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
      | (LABEL)? 'END' seos
      ;
   
   /* 15 */
   dimensionStatement
      : 'dimension' arrayDeclarators
      | 'DIMENSION' arrayDeclarators
      ;
   
   /* 16 */
   arrayDeclarator
      : (NAME | 'real') LPAREN arrayDeclaratorExtents RPAREN
      | (NAME | 'REAL') LPAREN arrayDeclaratorExtents RPAREN
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
      | 'EQUIVALENCE' equivEntityGroup (COMMA equivEntityGroup)*
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
      | 'COMMON' (commonBlock (COMMA commonBlock)* | commonItems)
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
      : (('real' | 'REAL') | ('complex' | 'COMPLEX') (STAR ICON?)? | 'double' 'complex' | 'DOUBLE' 'COMPLEX' | 'double' 'precision' | 'DOUBLE' 'PRECISION' | ('integer' | 'INTEGER') | ('logical' | 'LOGICAL'))
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
      | 'POINTER' pointerDecl (COMMA pointerDecl)*
      ;
   
   pointerDecl
      : LPAREN NAME COMMA NAME RPAREN
      ;
   
   /* 21 */
   implicitStatement
      : 'implicit' (implicitNone | implicitSpecs)
      | 'IMPLICIT' (implicitNone | implicitSpecs)
      ;
   
   implicitSpec
      : type LPAREN implicitLetters RPAREN
      ;
   
   implicitSpecs
      : implicitSpec (COMMA implicitSpec)*
      ;
   
   implicitNone
      : 'none'
      | 'NONE'
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
      | 'CHARACTER' (cwlLen)?
      ;
   
   cwlLen
      : STAR lenSpecification
      ;
   
   /* 23 */
   parameterStatement
      : 'parameter' LPAREN paramlist RPAREN
      | 'PARAMETER' LPAREN paramlist RPAREN
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
      | 'EXTERNAL' namelist
      ;
   
   /* 25 */
   intrinsicStatement
      : 'intrinsic' namelist
      | 'INTRINSIC' namelist
      ;
   
   /* 26 */
   saveStatement
      : 'save' (saveEntity (COMMA saveEntity)*)?
      | 'SAVE' (saveEntity (COMMA saveEntity)*)?
      ;
   
   saveEntity
      : (NAME | DIV NAME DIV)
      ;
   
   /* 27 */
   dataStatement
      : 'data' dataStatementEntity ((COMMA)? dataStatementEntity)*
      | 'DATA' dataStatementEntity ((COMMA)? dataStatementEntity)*
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
   //assignmentStatement
   // : varRef ASSIGN expression
   //|
   //	'let' varRef ASSIGN expression | 'assign' ICON to variableName
   //;
   /* 30 */
   gotoStatement
      : ('goto' | 'go' to) (unconditionalGoto | computedGoto | assignedGoto)
      | ('GOTO' | 'GO' to) (unconditionalGoto | computedGoto | assignedGoto)
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
      | 'IF' LPAREN logicalExpression RPAREN (blockIfStatement | logicalIfStatement | arithmeticIfStatement)
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
      | 'THEN' seos (wholeStatement) +
      ;
   
   /* 37 */
   elseIfStatement
      : ('elseif' | 'else' 'if') LPAREN logicalExpression RPAREN 'then' seos (wholeStatement)*
      | ('ELSEIF' | 'ELSE' 'IF') LPAREN logicalExpression RPAREN 'then' seos (wholeStatement)*
      ;
   
   /* 38 */
   elseStatement
      : 'else' seos (wholeStatement)*
      | 'ELSE' seos (wholeStatement)*
      ;
   
   /* 39 */
   endIfStatement
      : ('endif' | 'end' 'if')
      | ('ENDIF' | 'END' 'IF')
      ;
   
   /* 40 */
   doStatement
      : 'do' (doWithLabel | doWithEndDo)
      | 'DO' (doWithLabel | doWithEndDo)
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
      | ('ENDDO' | 'END' 'DO')
      ;
   
   /* 41 */
   continueStatement
      : 'continue'
      | 'CONTINUE'
      ;
   
   /* 42 */
   stopStatement
      : 'stop' (ICON | HOLLERITH)?
      | 'STOP' (ICON | HOLLERITH)?
      ;
   
   /* 43 */
   pauseStatement
      : 'pause' (ICON | HOLLERITH)
      | 'PAUSE' (ICON | HOLLERITH)
      ;
   
   /* 44 */
   writeStatement
      : 'write' LPAREN controlInfoList RPAREN ((COMMA ioList) +)?
      | 'WRITE' LPAREN controlInfoList RPAREN ((COMMA ioList) +)?
      ;
   
   /* 45 */
   readStatement
      : 'read' (formatIdentifier ((COMMA ioList) +)?)
      | 'READ' (formatIdentifier ((COMMA ioList) +)?)
      ;
   
   /* 46 */
   printStatement
      : 'print' (formatIdentifier ((COMMA ioList) +)?)
      | 'PRINT' (formatIdentifier ((COMMA ioList) +)?)
      ;
   
   assignmentStatement
      : varRef ASSIGN expression
      |
      //	'let' varRef ASSIGN expression | 'assign' ICON to variableName
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
      | 'OPEN' LPAREN openControl (COMMA openControl)* RPAREN
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
      | 'FMT'
      ;
   
   controlUnit
      : 'unit'
      | 'UNIT'
      ;
   
   controlRec
      : NAME
      ;
   
   controlEnd
      : 'end'
      | 'END'
      ;
   
   controlErr
      : 'err'
      | 'ERR'
      ;
   
   controlIostat
      : 'iostat'
      | 'IOSTAT'
      ;
   
   controlFile
      : 'file'
      | 'FILE'
      ;
   
   controlStatus
      : 'status'
      | 'STATUS'
      ;
   
   controlAccess
      : 'access'
      | 'ACCESS'
      ;
   
   controlPosition
      : 'position'
      | 'POSITION'
      ;
   
   controlForm
      : 'form'
      | 'FORM'
      ;
   
   controlRecl
      : 'recl'
      | 'RECL'
      ;
   
   controlBlank
      : 'blank'
      | 'BLANK'
      ;
   
   controlExist
      : 'exist'
      | 'EXIST'
      ;
   
   controlOpened
      : 'opened'
      | 'OPENED'
      ;
   
   controlNumber
      : 'number'
      | 'NUMBER'
      ;
   
   controlNamed
      : 'named'
      | 'NAMED'
      ;
   
   controlName
      : 'name'
      | 'NAME'
      ;
   
   controlSequential
      : 'sequential'
      | 'SEQUETIAL'
      ;
   
   controlDirect
      : NAME
      ;
   
   controlFormatted
      : 'formatted'
      | 'FORMATTED'
      ;
   
   controlUnformatted
      : 'unformatted'
      | 'UNFORMATTED'
      ;
   
   controlNextrec
      : 'nextrec'
      | 'NEXTREC'
      ;
   
   /* 51 */
   closeStatement
      : 'close' LPAREN closeControl (COMMA closeControl)* RPAREN
      | 'CLOSE' LPAREN closeControl (COMMA closeControl)* RPAREN
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
      | 'INQUIRE' LPAREN inquireControl (COMMA inquireControl)* RPAREN
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
      | 'BACKSPACE' berFinish
      ;
   
   /* 54 */
   endfileStatement
      : 'endfile' berFinish
      | 'ENDFILE' berFinish
      ;
   
   /* 55 */
   rewindStatement
      : 'rewind' berFinish
      | 'REWIND' berFinish
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
      | 'FORMAT' LPAREN fmtSpec RPAREN
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
      | 'LET' sfArgs ASSIGN expression
      ;
   
   sfArgs
      : NAME LPAREN namelist RPAREN
      ;
   
   /* 71 */
   callStatement
      : 'call' subroutineCall
      | 'CALL' subroutineCall
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
      | 'RETURN' (integerExpr)?
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
      | ('REAL')
      ;
   
   to
      : NAME
      ;
   
   // keyword contains all of the FORTRAN keywords.
   keyword
      : 'program'
      | 'PROGRAM'
      | 'entry'
      | 'ENTRY'
      | 'function'
      | 'FUNCTION'
      | 'block'
      | 'BLOCK'
      | 'subroutine'
      | 'SUBROUTINE'
      | 'end'
      | 'END'
      | 'dimension'
      | 'DIMENSION'
      | 'equivalence'
      | 'EQUIVALENCE'
      | 'common'
      | 'COMMON'
      | 'real' 'complex'
      | 'REAL' 'COMPLEX'
      | 'double'
      | 'DOUBLE'
      | 'precision'
      | 'PRECISION'
      | 'integer'
      | 'INTEGER'
      | 'logical'
      | 'LOGICAL'
      | 'pointer'
      | 'POINTER'
      | 'implicit'
      | 'IMPLICIT'
      | 'none'
      | 'NONE'
      | 'character'
      | 'CHARACTER'
      | 'parameter'
      | 'PARAMETER'
      | 'external'
      | 'EXTERNAL'
      | 'intrinsic'
      | 'INTRINSIC'
      | 'save'
      | 'SAVE'
      | 'data'
      | 'DATA'
      | 'assign'
      | 'ASSIGN'
      | 'go'
      | 'GO'
      | 'if'
      | 'IF'
      | 'then'
      | 'THEN'
      | 'elseif'
      | 'ELSEIF'
      | 'else'
      | 'ELSE'
      | 'endif'
      | 'ENDIF'
      | 'do'
      | 'DO'
      | 'enddo'
      | 'ENDDO'
      | 'continue'
      | 'CONTINUE'
      | 'stop'
      | 'STOP'
      | 'pause'
      | 'PAUSE'
      | 'write'
      | 'WRITE'
      | 'read'
      | 'READ'
      | 'print'
      | 'PRINT'
      | 'open'
      | 'OPEN'
      | 'fmt'
      | 'FMT'
      | 'unit'
      | 'UNIT'
      | 'iostat'
      | 'IOSTAT'
      | 'file'
      | 'FILE'
      | 'status'
      | 'STATUS'
      | 'access'
      | 'ACCESS'
      | 'position'
      | 'POSITION'
      | 'form'
      | 'FORM'
      | 'recl'
      | 'RECL'
      | 'blank'
      | 'BLANK'
      | 'exist'
      | 'EXIST'
      | 'opened'
      | 'OPENED'
      | 'number'
      | 'NUMBER'
      | 'named'
      | 'NAMED'
      | 'name'
      | 'NAME'
      | 'sequential'
      | 'SEQUENTIAL'
      | 'unformatted'
      | 'UNFORMATTED'
      | 'nextrec'
      | 'NEXTREC'
      | 'close'
      | 'CLOSE'
      | 'inquire'
      | 'INQUIRE'
      | 'backspace'
      | 'BACKSPACE'
      | 'endfile'
      | 'ENDFILE'
      | 'rewind'
      | 'REWIND'
      | 'format'
      | 'FORMAT'
      | 'let'
      | 'LET'
      | 'call'
      | 'CALL'
      | 'return'
      | 'RETURN'
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
      : '.not.' | '.NOT.'
      ;
   
   
   LAND
      : '.and.' | '.AND.'
      ;
   
   
   LOR
      : '.or.' | '.OR.'
      ;
   
   
   EQV
      : '.eqv.' | '.EQV.'
      ;
   
   
   NEQV
      : '.neqv.' | '.NEQV.'
      ;
   
   
   XOR
      : '.xor.' | '.XOR.'
      ;
   
   
   EOR
      : '.eor.' | '.EOR.'
      ;
   
   
   LT
      : '.lt.' | '.LT.'
      ;
   
   
   LE
      : '.le.' | '.LE.'
      ;
   
   
   GT
      : '.gt.' | '.GT.'
      ;
   
   
   GE
      : '.ge.' | '.GE.'
      ;
   
   
   NE
      : '.ne.' | '.NE.'
      ;
   
   
   EQ
      : '.eq.' | '.EQ.'
      ;
   
   
   TRUE
      : '.true.' | '.TRUE.'
      ;
   
   
   FALSE
      : '.false.' | '.FALSE.'
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
   
   /*RCON
   : 'RCON'
   ;
*/
   
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
      : (('\r')? '\n')
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
      : ('c' | '*' | '!')
      ;
   
   // '' is used to drop the charater when forming the lexical token
   // Strings are assumed to start with a single quote (') and two
   // single quotes is meant as a literal single quote
   
   SCON
      : '\'' ('\'' '\'' | ~ ('\'' | '\n' | '\r') | (('\n' | '\r' ('\n')?) '     ' CONTINUATION) ('\n' | '\r' ('\n')?) '     ' CONTINUATION)* '\''
      ;
   
   // numeral literal: ICON goes here what to do what to do?
   
   RCON
      : '.' (NUM)* (EXPON)?
      ;
   
   
   ZCON
      : 'z' '\'' (HEX) + '\''
      ;
   
   // identifier (keyword or variable)
   
   NAME
      : (('i' | 'f' | 'd' | 'g' | 'e') (NUM) + '.') FDESC | (ALNUM +) (ALNUM)*
      ;
   
   
   WHITE
      : (' ' | '\t')
      ;
   
   
   ALPHA
      : ('a' .. 'z') | ('A' .. 'Z')
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
   