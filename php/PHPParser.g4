/*
 [The "BSD licence"]
 Copyright (c) 2013 Tom Everett
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

//
//
// HTML Grammar based on the ANTLR4 XML Grammar by Terence Parr
//
//
parser grammar PHPParser;

options { tokenVocab=PHPLexer; }

htmlDocument    
    : htmlDTD? htmlElements*
    ;

htmlDTD
    : HTML_DTD
    ;

htmlElements
    : htmlMisc* (htmlElement | phpBlock) htmlMisc*
    ;

htmlElement     
    : '<' HTML_Name htmlAttribute* '>' htmlContent '<' HTML_SLASH HTML_Name '>'
    | '<' HTML_Name htmlAttribute* '/>'
    | '<' HTML_Name htmlAttribute* '>'
    ;

htmlContent     
    : htmlChardata? ((htmlElement | htmlReference | htmlComment | phpBlock) htmlChardata?)*
    ;

htmlReference   
    : HTML_EntityRef 
    | HTML_CharRef
    ;

htmlAttribute   
    : HTML_Name HTML_EQUALS htmlLiteral
    ;

htmlLiteral
    : HTML_QUOTED_STRING 
//    | HTML_UNQUOTED_STRING
    | HTML_HEX
    ;

htmlChardata    
    : HTML_TEXT 
    | SEA_WS
    ;

htmlMisc        
    : htmlComment 
    | SEA_WS
    ;

htmlComment
    : HTML_COMMENT
    ;

//
//
// PHP Grammar
//
//

phpBlock 
    : prolog statement* epilog?
    ;

prolog
    : PHP_Start
    ;

epilog
    : PHP_END
    ;

//
//
// statement
//
//

statement
    : '{' statement '}'
    | bracketedBlock
    | classDefinition
    | interfaceDefinition
    | complexStatement
    | simpleStatement ';'
    ;

complexStatement
    : ifstatement
    | forstatement 
    | foreachstatement
    | whilestatement
    | dostatement
    | switchstatement 
    | functionDefinition
    ;


forstatement
    : For '(' expression ';' expression ';'  expression ')' statement 
    ;

ifstatement
    : If '(' expression ')' statement (ElseIf '(' expression ')' statement)* (Else statement)?
    ;

foreachstatement
    : Foreach '(' variable 'as' arrayEntry ')' statement 
    ;

whilestatement
    : While '(' expression? ')' statement
    ;

dostatement
    : Do statement While '(' expression ')' ';' 
    ;

switchstatement
    : Switch '(' expression ')' '{'cases'}' 
    ;

simpleStatement
    : globalStatement
    | staticVariableAssignmentStatement
    | breakStatement
    | continueStatement
    | returnStatement
    | requireStatement
    | expression
    ;

globalStatement
    : Global name (Comma name)*
    ;

staticVariableAssignmentStatement
    : Static? variable Equals expression
    ;

breakStatement
    : Break DecimalNumber?
    ;    

continueStatement
    : Continue DecimalNumber?
    ;

returnStatement
    : Return expression?
    ;

requireStatement
    : RequireOperator expression
    ;

cases 
    : casestatement*  defaultcase
    ;

casestatement
    : Case expression Colon statement*
    ;

defaultcase 
    : (Default Colon statement*)
    ;

//
//
// variable
//
//

variable
    : Dollar variablename
    | Dollar variablename InstanceMember variablename
    ;

variablename
    : Identifier
    ;

//
//
// name
//
//

name: staticMemberAccess
    | memberAccess
    | variable
    ;
    
staticMemberAccess
    : Identifier ClassMember variable
    ;

memberAccess
    : variable ( OpenSquareBracket expression CloseSquareBracket)*
    ;

//
//
// block
//
//

bracketedBlock
    : '{' statement* '}'
    ;

//
//
// interface
//
//

interfaceDefinition
    : Interface interfaceName interfaceExtends?
        OpenCurlyBracket
        interfaceMember*
        CloseCurlyBracket
    ;

interfaceName
    : Identifier
    ;

interfaceExtends
    : Extends interfaceName (Comma interfaceName)*
    ;
interfaceMember
    : Const Identifier (Equals atom)? ';' 
    | fieldModifier* Function functionName parametersDefinition ';'
    ;

//
//
// class
//
//

className
    : Identifier
    ;

classDefinition
    :   classModifier? 
        Class className 
        (Extends className)? 
        classImplements?
        OpenCurlyBracket
        classMember*
        CloseCurlyBracket 
    ;
    
classImplements
    :  Implements (interfaceName (Comma interfaceName)*)
    ;

classMember
    : fieldModifier* Function functionName parametersDefinition (bracketedBlock | ';')
    | constDefinition ';' 
    | Var? fieldModifier* fieldDefinition ';'      
    ;

classModifier
    : Abstract;

//
//
// const
//
//
constDefinition
    : Const variablename Equals atom;

//
//
// field
//
//

fieldDefinition
    : fieldName (Equals atom)? 
    ;
    
fieldModifier
    : AccessModifier | Abstract | Static 
    ;

fieldName
    : Dollar variablename
    ;

//
//
// functions
//
//

functionDefinition
    : Function functionName parametersDefinition bracketedBlock 
    ;

parametersDefinition
    : OpenRoundBracket (paramDef (Comma paramDef)*)? CloseRoundBracket 
    ;

functionInvocation
    : functionName functionInvocationParameters
    ;

functionName
    : Identifier
    ;

functionInvocationParameters
    : OpenRoundBracket? (commaList)? CloseRoundBracket? 
    ;

paramDef
    : paramName (Equals atom)?
    ;

paramName
    : Dollar Identifier
    | Ampersand Dollar Identifier 
    ;

commaList
    : expression (',' expression)* 
    ;
    
//
//
// expression
//
//

expression
    : weakLogicalOr 
    ;

weakLogicalOr
    : weakLogicalXor (Or weakLogicalXor)*
    ;

weakLogicalXor
    : weakLogicalAnd (Xor weakLogicalAnd)*
    ;
    
weakLogicalAnd
    : assignment (And assignment)*
    ;

assignment
    : listVariables ((Equals | AssignmentOperator) assignment) 
    | ternary
    ;

listVariables
    : List OpenRoundBracket name (',' name)* CloseRoundBracket 
    | name
    ;

ternary
    : logicalOr QuestionMark expression Colon expression 
    | logicalOr
    ;
    
logicalOr
    : logicalAnd (LogicalOr logicalAnd)*
    ;

logicalAnd
    : bitwiseOr (LogicalAnd bitwiseOr)*
    ;
    
bitwiseOr
    : bitWiseAnd (Pipe bitWiseAnd)*
    ;

bitWiseAnd
    : equalityCheck (Ampersand equalityCheck)*
    ;

equalityCheck
    : comparisionCheck (EqualityOperator comparisionCheck)?
    ;
    
comparisionCheck
    : bitWiseShift (ComparisionOperator bitWiseShift)?
    ;

bitWiseShift
    : addition (ShiftOperator addition)*
    ;
    
addition
    : multiplication ((Plus | Minus | Dot) multiplication)*
    ;

multiplication
    : logicalNot ((Asterisk | Forwardslash | Percent) logicalNot)*
    ;

logicalNot
    : Bang logicalNot
    | instanceOf
    ;

instanceOf
    : negateOrCast (Instanceof negateOrCast)?
    ;

negateOrCast
    : (Tilde | Minus | SuppressWarnings) increment
    | OpenRoundBracket PrimitiveType CloseRoundBracket increment 
    | OpenRoundBracket weakLogicalAnd CloseRoundBracket
    | increment
    ;

increment
    : IncrementOperator name 
    | name IncrementOperator
    | newOrClone
    ;

newOrClone
    : New name
    | Clone name
    | atomOrReference
    ;

atomOrReference
    : atom
    | reference
    ;

arrayDeclaration
    : Array OpenRoundBracket (arrayEntry (Comma arrayEntry)*)? CloseRoundBracket 
    ;

arrayEntry
    : (keyValuePair | expression)
    ;

keyValuePair
    : (expression ArrayAssign expression) 
    ;

atom: number
    | string 
    | bool
    | arrayDeclaration 
    | functionInvocation
    ;

reference
    : Ampersand name
    | name
    ;

bool
    : Boolean
    ;

number
    : integer  
    | real
    ; 

integer 
    : DecimalNumber
    | HexNumber
    | OctalNumber
    ;

real: Float ( RealE (Float | DecimalNumber))*
    ;
    
string
    : SingleQuotedString
    | doubleQuotedString
    ;

doubleQuotedString
    : DoubleQuotedString_Start doubleQuotedStringBody? DoubleQuotedString_End
    ;

doubleQuotedStringBody
    : (DoubleQuotedStringBody | VarName)*
    ;
