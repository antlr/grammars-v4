/*
 * Copyright (c) Matt Quinn and The OpenPplSoft Runtime Project (http://openpplsoft.org)
 * All rights reserved.
 *
 * MIT License
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies
 * or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

grammar PeopleCode;

//******************************************************//
// Parser Rules                                         //
//******************************************************//

program : stmtList ;

// Multiple semicolons may be present; the last/only statement may not have a semicolon.
stmtList: (stmt ';'+)* stmt? ;   

stmt  : appClassImport          # StmtAppClassImport
      | extFuncImport           # StmtExternalFuncImport
      | classDeclaration        # StmtClassDeclaration
      | methodImpl              # StmtMethodImpl
      | getImpl                 # StmtGetImpl
      | setImpl                 # StmtSetImpl
      | funcImpl                # StmtFuncImpl
      | varDeclaration          # StmtVarDeclaration
      | ifStmt                  # StmtIf
      | forStmt                 # StmtFor
      | whileStmt               # StmtWhile
      | evaluateStmt            # StmtEvaluate
      | tryCatchStmt            # StmtTryCatch
      | 'Exit'                  # StmtExit
      | 'Break'                 # StmtBreak
      | 'Error' expr            # StmtError
      | 'Warning' expr          # StmtWarning
      | 'Return' expr?          # StmtReturn
      | 'throw' expr            # StmtThrow
      | expr '=' expr           # StmtAssign  // Must be higher than fn call to properly resolve '=' ambiguity.
      | expr                    # StmtExpr    // For fn calls; the value of expr is not assigned to anything.
      ;

expr  : '(' expr ')'                              # ExprParenthesized
      | '@' expr                                  # ExprDynamicReference
      | literal                                   # ExprLiteral
      | id                                        # ExprId
      | createInvocation                          # ExprCreate
      | expr '.' id                               # ExprDotAccess
      | expr '[' exprList ']'                     # ExprArrayIndex // &array[&i, &j] is shorthand for &array[&i][&j]
      | expr '(' exprList? ')'                    # ExprFnOrIdxCall
      | '-' expr                                  # ExprNegate
      | 'Not' expr                                # ExprNot
      | expr (m='*'|d='/') expr                   # ExprMulDiv
      | expr (a='+'|s='-') expr                   # ExprAddSub
      | expr (le='<='|ge='>='|l='<'|g='>') expr   # ExprComparison
      | expr (e='='|i='<>') expr                  # ExprEquality
      | expr (
            <assoc=right>op='And'
          | <assoc=right>op='Or'
        ) expr                                    # ExprBoolean // order of precedence: Not, And, then Or
      | expr '|' expr                             # ExprConcat
      ;

exprList: expr (',' expr)* ;

varDeclaration  : varScope=GENERIC_ID varType varDeclarator (',' varDeclarator)* ;
varDeclarator : VAR_ID ('=' expr)? ;

// - The first alt catches primitive types (i.e., "any", "integer"), complex types
//   (i.e., "Record"), arrays of any dimension (i.e., "array of array of Record"),
//   and app class names without path prefixes (i.e., "Address" for "EO:CA:Address").
// - Note: the 'of' clause is only valid after "array" - I'm opting to enforce this
//   at runtime rather than syntactically.
varType   : GENERIC_ID ('of' varType)? | appClassPath ;

appClassImport  : 'import' (appPkgPath|appClassPath) ;
appPkgPath      : GENERIC_ID (':' GENERIC_ID)* ':' '*' ;
appClassPath    : GENERIC_ID (':' GENERIC_ID)+ ;

extFuncImport   : 'Declare' 'Function' GENERIC_ID 'PeopleCode' recDefnPath event ;
recDefnPath     : GENERIC_ID '.' GENERIC_ID ;
event           : 'FieldFormula' | 'FieldChange' ;

classDeclaration  : 'class' GENERIC_ID classBlock* 'end-class' ;
classBlock        : aLvl='private'? (classBlockStmt ';'*)+ ;
classBlockStmt    : method | constant | property | instance ;
method            : 'method' GENERIC_ID formalParamList returnType? ;
constant          : 'Constant' VAR_ID '=' expr ;
property          : 'property' varType GENERIC_ID g='get'? s='set'? r='readonly'? ;
instance          : 'instance' varType VAR_ID (',' VAR_ID)* ;

methodImpl  : 'method' GENERIC_ID stmtList endmethod='end-method' ;
getImpl     : 'get' GENERIC_ID stmtList endget='end-get' ;
setImpl     : 'set' GENERIC_ID stmtList endset='end-set' ;

funcImpl        : funcSignature stmtList endfunction='End-Function' ;
funcSignature   : 'Function' GENERIC_ID formalParamList? returnType? ';'? ;
formalParamList : '(' ( param (',' param)* )? ')' ;
param           : VAR_ID ('As' varType)? ;
returnType      : 'Returns' varType ;

ifStmt  : 'If' expr 'Then' ';'? stmtList (elsetok='Else' ';'? stmtList)? endif='End-If' ;

forStmt : 'For' VAR_ID '=' expr 'To' expr (';' | ('Step' expr))? stmtList endfor='End-For' ;

whileStmt : 'While' expr ';'? stmtList 'End-While' ;

evaluateStmt    : 'Evaluate' expr whenBranch+ whenOtherBranch? endevaluate='End-Evaluate' ;
whenBranch      : 'When' (op='='|op='>')? expr stmtList ;
whenOtherBranch : 'When-Other' stmtList ;

tryCatchStmt    : trytok='try' stmtList catchSignature stmtList endtry='end-try' ;
catchSignature  : 'catch' exClass='Exception' VAR_ID ;

createInvocation : 'create' (appClassPath|GENERIC_ID) '(' exprList? ')' ;

literal : DecimalLiteral
        | IntegerLiteral
        | StringLiteral
        | BoolLiteral
        ;

id  : SYS_VAR_ID | VAR_ID | GENERIC_ID ;

//*******************************************************//
// Lexer Rules                                           //
//*******************************************************//

DecimalLiteral  : IntegerLiteral '.' [0-9]+ ;
IntegerLiteral  : '0' | '1'..'9' '0'..'9'* ;
StringLiteral   : '"' ( ~'"' )* '"' ;
BoolLiteral     :   'True' | 'False' ;

VAR_ID      : '&' GENERIC_ID ;
SYS_VAR_ID  : '%' GENERIC_ID ;
GENERIC_ID  : [a-zA-Z] [0-9a-zA-Z_#]* ;

REM       : WS? [rR][eE][mM] WS .*? ';' -> skip;
COMMENT_1 : '/*' .*? '*/' -> skip;
COMMENT_2 : '<*' .*? '*>' -> skip;
COMMENT_3 : '/+' .*? '+/' ';'? -> skip;
WS        : [ \t\r\n]+ -> skip;