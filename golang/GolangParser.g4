/*
 [The "BSD licence"]
 Copyright (c) 2017 Sasa Coh, Michał Błotniak
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
/**
 * A Go grammar for ANTLR 4 derived from the Go Language Specification
 * https://golang.org/ref/spec
 *
 */
parser grammar GolangParser;

options {
    tokenVocab=GolangLexer;
    superClass=GoBaseParser;
}

sourceFile
    : packageClause eos ( importDecl eos )* ( topLevelDecl eos )*
    ;

//PackageClause  = "package" PackageName .
//PackageName    = identifier .
packageClause
    : Package IDENTIFIER
    ;

importDecl
    : Import ( importSpec | '(' ( importSpec eos )* ')' )
    ;

importSpec
    : ( '.' | IDENTIFIER )? importPath
    ;

importPath
    : STRING_LIT
    ;

//TopLevelDecl  = Declaration | FunctionDecl | MethodDecl .
topLevelDecl
    : declaration
    | functionDecl
    | methodDecl
    ;

//Declaration   = ConstDecl | TypeDecl | VarDecl .
declaration
    : constDecl
    | typeDecl
    | varDecl
    ;


//ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
constDecl
    : Const ( constSpec | '(' ( constSpec eos )* ')' )
    ;

//ConstSpec      = IdentifierList [ [ Type ] "=" ExpressionList ] .
constSpec
    : identifierList ( type_? '=' expressionList )?
    ;

//
//IdentifierList = identifier { "," identifier } .
identifierList
    : IDENTIFIER ( ',' IDENTIFIER )*
    ;

//ExpressionList = Expression { "," Expression } .
expressionList
    : expression ( ',' expression )*
    ;

//TypeDecl     = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
typeDecl
    : Type ( typeSpec | '(' ( typeSpec eos )* ')' )
    ;

//TypeSpec     = identifier Type .
typeSpec
    : IDENTIFIER type_
    ;


// Function declarations

//FunctionDecl = "func" FunctionName ( Function | Signature ) .
//FunctionName = identifier .
//Function     = Signature FunctionBody .
//FunctionBody = Block .
functionDecl
    : Func IDENTIFIER ( function | signature )
    ;

function
    : signature block
    ;

//MethodDecl   = "func" Receiver MethodName ( Function | Signature ) .
//Receiver     = Parameters .
methodDecl
    : Func receiver IDENTIFIER ( function | signature )
    ;

receiver
    : parameters
    ;

//VarDecl     = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
//VarSpec     = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
varDecl
    : Var ( varSpec | '(' ( varSpec eos )* ')' )
    ;

varSpec
    : identifierList ( type_ ( '=' expressionList )? | '=' expressionList )
    ;


//Block = "{" StatementList "}" .
block
    : '{' statementList '}'
    ;

//StatementList = { Statement ";" } .
statementList
    : ( statement eos )*
    ;

statement
    : declaration
    | labeledStmt
    | simpleStmt
    | goStmt
    | returnStmt
    | breakStmt
    | continueStmt
    | gotoStmt
    | fallthroughStmt
    | block
    | ifStmt
    | switchStmt
    | selectStmt
    | forStmt
    | deferStmt
	;

//SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment | ShortVarDecl .
simpleStmt
    : sendStmt
    | expressionStmt
    | incDecStmt
    | assignment
    | shortVarDecl
    | emptyStmt
    ;

//ExpressionStmt = Expression .
expressionStmt
    : expression
    ;

//SendStmt = Channel "<-" Expression .
//Channel  = Expression .
sendStmt
    : expression ChanOp expression
    ;

//IncDecStmt = Expression ( "++" | "--" ) .
incDecStmt
    : expression ( PlusPlus | MinusMinus )
    ;

//Assignment = ExpressionList assign_op ExpressionList .
assignment
    : expressionList assign_op expressionList
    ;

//assign_op = [ add_op | mul_op ] "=" .
assign_op
    : (Plus | Minus | BitOr | BitXor | Multiply | Divide | Modulus | LeftShiftArithmetic | RightShiftArithmetic | BitAnd | BitClear)? '='
    ;


//ShortVarDecl = IdentifierList ":=" ExpressionList .
shortVarDecl
    : identifierList ':=' expressionList
    ;

emptyStmt
    : ';'
    ;

//LabeledStmt = Label ":" Statement .
//Label       = identifier .
labeledStmt
    : IDENTIFIER ':' statement
    ;

//ReturnStmt = "return" [ ExpressionList ] .
returnStmt
    : Return expressionList?
    ;

//BreakStmt = "break" [ Label ] .
breakStmt
    : Break IDENTIFIER?
    ;

//ContinueStmt = "continue" [ Label ] .
continueStmt
    : Continue IDENTIFIER?
    ;

//GotoStmt = "goto" Label .
gotoStmt
    : Goto IDENTIFIER
    ;

//FallthroughStmt = "fallthrough" .
fallthroughStmt
    : Fallthrough
    ;

//DeferStmt = "defer" Expression .
deferStmt
    : Defer expression
    ;

//IfStmt = "if" [ SimpleStmt ";" ] Expression Block [ "else" ( IfStmt | Block ) ] .
ifStmt
    : If (simpleStmt ';')? expression block ( Else ( ifStmt | block ) )?
    ;

//SwitchStmt = ExprSwitchStmt | TypeSwitchStmt .
switchStmt
    : exprSwitchStmt | typeSwitchStmt
    ;

//ExprSwitchStmt = "switch" [ SimpleStmt ";" ] [ Expression ] "{" { ExprCaseClause } "}" .
//ExprCaseClause = ExprSwitchCase ":" StatementList .
//ExprSwitchCase = "case" ExpressionList | "default" .
exprSwitchStmt
    : Switch ( simpleStmt ';' )? expression? '{' exprCaseClause* '}'
    ;

exprCaseClause
    : exprSwitchCase ':' statementList
    ;

exprSwitchCase
    : Case expressionList | Default
    ;

//TypeSwitchStmt  = "switch" [ SimpleStmt ";" ] TypeSwitchGuard "{" { TypeCaseClause } "}" .
//TypeSwitchGuard = [ identifier ":=" ] PrimaryExpr "." "(" "type" ")" .
//TypeCaseClause  = TypeSwitchCase ":" StatementList .
//TypeSwitchCase  = "case" TypeList | "default" .
//TypeList        = Type { "," Type } .
typeSwitchStmt
    : Switch ( simpleStmt ';' )? typeSwitchGuard '{' typeCaseClause* '}'
    ;
typeSwitchGuard
    : ( IDENTIFIER ':=' )? primaryExpr '.' '(' Type ')'
    ;
typeCaseClause
    : typeSwitchCase ':' statementList
    ;
typeSwitchCase
    : Case typeList | Default
    ;
typeList
    : type_ ( ',' type_ )*
    ;


//SelectStmt = "select" "{" { CommClause } "}" .
//CommClause = CommCase ":" StatementList .
//CommCase   = "case" ( SendStmt | RecvStmt ) | "default" .
//RecvStmt   = [ ExpressionList "=" | IdentifierList ":=" ] RecvExpr .
//RecvExpr   = Expression .
selectStmt
    : Select '{' commClause* '}'
    ;
commClause
    : commCase ':' statementList
    ;
commCase
    : Case ( sendStmt | recvStmt ) | Default
    ;
recvStmt
    : ( expressionList '=' | identifierList ':=' )? expression
    ;

//ForStmt = "for" [ Condition | ForClause | RangeClause ] Block .
//Condition = Expression .
forStmt
    : For ( expression | forClause | rangeClause )? block
    ;

//ForClause = [ InitStmt ] ";" [ Condition ] ";" [ PostStmt ] .
//InitStmt = SimpleStmt .
//PostStmt = SimpleStmt .
forClause
    : simpleStmt? ';' expression? ';' simpleStmt?
    ;


//RangeClause = [ ExpressionList "=" | IdentifierList ":=" ] "range" Expression .
rangeClause
    : (expressionList '=' | identifierList ':=' )? Range expression
    ;

//GoStmt = "go" Expression .
goStmt
    : Go expression
    ;

//Type      = TypeName | TypeLit | "(" Type ")" .
type_
    : typeName
    | typeLit
    | '(' type_ ')'
    ;

//TypeName  = identifier | QualifiedIdent .
typeName
    : IDENTIFIER
    | qualifiedIdent
    ;

//TypeLit   = ArrayType | StructType | PointerType | FunctionType | InterfaceType |
//	    SliceType | MapType | ChannelType .
typeLit
    : arrayType
    | structType
    | pointerType
    | functionType
    | interfaceType
    | sliceType
    | mapType
    | channelType
    ;


arrayType
    : '[' arrayLength ']' elementType
    ;

arrayLength
    : expression
    ;

elementType
    : type_
    ;

//PointerType = "*" BaseType .
//BaseType    = Type .
pointerType
    : '*' type_
    ;

//InterfaceType      = "interface" "{" { MethodSpec ";" } "}" .
//MethodSpec         = MethodName Signature | InterfaceTypeName .
//MethodName         = identifier .
//InterfaceTypeName  = TypeName .
interfaceType
    : Interface '{' ( methodSpec eos )* '}'
    ;

//SliceType = "[" "]" ElementType .
sliceType
    : '[' ']' elementType
    ;

//MapType     = "map" "[" KeyType "]" ElementType .
//KeyType     = Type .
mapType
    : Map '[' type_ ']' elementType
    ;

//ChannelType = ( "chan" | "chan" "<-" | "<-" "chan" ) ElementType .
channelType
    : ( Chan | Chan ChanOp | ChanOp Chan ) elementType
    ;

methodSpec
    : {noTerminatorAfterParams(2)}? IDENTIFIER parameters result
    | typeName
    | IDENTIFIER parameters
    ;


//FunctionType   = "func" Signature .
//Signature      = Parameters [ Result ] .
//Result         = Parameters | Type .
//Parameters     = "(" [ ParameterList [ "," ] ] ")" .
//ParameterList  = ParameterDecl { "," ParameterDecl } .
//ParameterDecl  = [ IdentifierList ] [ "..." ] Type .
functionType
    : Func signature
    ;

signature
    : {noTerminatorAfterParams(1)}? parameters result
    | parameters
    ;

result
    : parameters
    | type_
    ;

parameters
    : '(' ( parameterList ','? )? ')'
    ;

parameterList
    : parameterDecl ( ',' parameterDecl )*
    ;

parameterDecl
    : identifierList? Ellipsis? type_
    ;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Operands

//Operand     = Literal | OperandName | MethodExpr | "(" Expression ")" .
//Literal     = BasicLit | CompositeLit | FunctionLit .
//BasicLit    = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
//OperandName = identifier | QualifiedIdent.

operand
    : literal
    | operandName
    | methodExpr
    | '(' expression ')'
    ;

literal
    : basicLit
    | compositeLit
    | functionLit
    ;

basicLit
    : INT_LIT
    | FLOAT_LIT
    | IMAGINARY_LIT
    | RUNE_LIT
    | STRING_LIT
    ;

operandName
    : IDENTIFIER
    | qualifiedIdent
    ;

//QualifiedIdent = PackageName "." identifier .
qualifiedIdent
    : IDENTIFIER '.' IDENTIFIER
    ;

//CompositeLit  = LiteralType LiteralValue .
//LiteralType   = StructType | ArrayType | "[" "..." "]" ElementType |
//                SliceType | MapType | TypeName .
//LiteralValue  = "{" [ ElementList [ "," ] ] "}" .
//ElementList   = KeyedElement { "," KeyedElement } .
//KeyedElement  = [ Key ":" ] Element .
//Key           = FieldName | Expression | LiteralValue .
//FieldName     = identifier .
//Element       = Expression | LiteralValue .

compositeLit
    : literalType literalValue
    ;

literalType
    : structType
    | arrayType
    | '[' Ellipsis ']' elementType
    | sliceType
    | mapType
    | typeName
    ;

literalValue
    : '{' ( elementList ','? )? '}'
    ;

elementList
    : keyedElement (',' keyedElement)*
    ;

keyedElement
    : (key ':')? element
    ;

key
    : IDENTIFIER
    | expression
    | literalValue
    ;

element
    : expression
    | literalValue
    ;

//StructType     = "struct" "{" { FieldDecl ";" } "}" .
//FieldDecl      = (IdentifierList Type | AnonymousField) [ Tag ] .
//AnonymousField = [ "*" ] TypeName .
//Tag            = string_lit .
structType
    : Struct '{' ( fieldDecl eos )* '}'
    ;

fieldDecl
    : ({noTerminatorBetween(2)}? identifierList type_ | anonymousField) STRING_LIT?
    ;

anonymousField
    : '*'? typeName
    ;

//FunctionLit = "func" Function .
functionLit
    : Func function
    ;

//PrimaryExpr =
//	Operand |
//	Conversion |
//	PrimaryExpr Selector |
//	PrimaryExpr Index |
//	PrimaryExpr Slice |
//	PrimaryExpr TypeAssertion |
//	PrimaryExpr Arguments .
//
//Selector       = "." identifier .
//Index          = "[" Expression "]" .
//Slice          = "[" ( [ Expression ] ":" [ Expression ] ) |
//                     ( [ Expression ] ":" Expression ":" Expression )
//                 "]" .
//TypeAssertion  = "." "(" Type ")" .
//Arguments      = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .

primaryExpr
    : operand
    | conversion
    | primaryExpr selector
    | primaryExpr index
    | primaryExpr slice
    | primaryExpr typeAssertion
	| primaryExpr arguments
    ;

selector
    : '.' IDENTIFIER
    ;

index
    : '[' expression ']'
    ;

slice
    : '[' (( expression? ':' expression? ) | ( expression? ':' expression ':' expression )) ']'
    ;

typeAssertion
    : '.' '(' type_ ')'
    ;

arguments
    : '(' ( ( expressionList | type_ ( ',' expressionList )? ) '...'? ','? )? ')'
    ;

//MethodExpr    = ReceiverType "." MethodName .
//ReceiverType  = TypeName | "(" "*" TypeName ")" | "(" ReceiverType ")" .
methodExpr
    : receiverType '.' IDENTIFIER
    ;

receiverType
    : typeName
    | '(' '*' typeName ')'
    | '(' receiverType ')'
    ;

//Expression = UnaryExpr | Expression binary_op Expression .
//UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .

expression
    : unaryExpr
//    | expression BINARY_OP expression
    | expression (Or | And | Equals_ | NotEquals | LessThan | LessThanEquals | GreaterThan | GreaterThanEquals | Plus | Minus | BitOr | BitXor | Multiply | Divide | Modulus | LeftShiftArithmetic | RightShiftArithmetic | BitAnd | BitClear) expression
    ;

unaryExpr
    : primaryExpr
    | (Plus|Minus|Not|BitXor|Multiply|BitAnd|ChanOp) unaryExpr
    ;

//Conversion = Type "(" Expression [ "," ] ")" .
conversion
    : type_ '(' expression ','? ')'
    ;

eos
    : ';'
    | EOF
    | {lineTerminatorAhead()}?
    | {closeBrace() }?
    ;
