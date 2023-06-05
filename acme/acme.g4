/*
BSD License

Copyright (c) 2020, Tom Everett
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
grammar acme;

acmeCompUnit
   : acmeImportDeclaration* (acmeSystemDeclaration | acmeFamilyDeclaration | acmeDesignDeclaration)+ EOF
   ;

acmeImportDeclaration
   : IMPORT (filename | stringLiteral) SEMICOLON
   ;

stringLiteral
   : STRING_LITERAL
   ;

filename
   : ('$' | '%')? IDENTIFIER (('.' | ':' | '-' | '+' | '\\' | '\\\\' | '/' | '$' | '%')+ IDENTIFIER)*
   ;

identifier
   : IDENTIFIER
   ;

codeLiteral
   : STRING_LITERAL
   ;

acmeFamilyDeclaration
   : (FAMILY | STYLE) identifier (SEMICOLON | ASSIGN acmeFamilyBody SEMICOLON? | EXTENDS acmeFamilyRef (COMMA acmeFamilyRef)* (SEMICOLON | WITH acmeFamilyBody SEMICOLON?))
   ;

acmeFamilyBody
   : LBRACE ( (PUBLIC | PRIVATE)? (FINAL | ABSTRACT)? (acmePortTypeDeclaration | acmeRoleTypeDeclaration | acmeComponentTypeDeclaration | acmeConnectorTypeDeclaration | acmeGenericElementTypeDeclaration | acmePropertyTypeDeclaration | acmeGroupTypeDeclaration) | acmeDesignAnalysisDeclaration | designRule | acmePortDeclaration | acmeRoleDeclaration | acmeComponentDeclaration | acmeConnectorDeclaration | acmePropertyDeclaration | acmeGroupDeclaration | acmeAttachmentDeclaration )* RBRACE
   ;

acmeSystemDeclaration
   : SYSTEM identifier (COLON acmeFamilyRef (COMMA acmeFamilyRef)*)? (SEMICOLON | ASSIGN (acmeSystemBody SEMICOLON? | NEW acmeFamilyInstantiationRef (COMMA acmeFamilyInstantiationRef)* (SEMICOLON | EXTENDED WITH acmeSystemBody SEMICOLON?)))
   ;

acmeSystemBody
   : LBRACE (acmePropertyDeclaration | acmeComponentDeclaration | acmeConnectorDeclaration | acmeAttachmentDeclaration | acmeGroupDeclaration | designRule)* RBRACE
   ;

acmeDesignDeclaration
   : DESIGN
   ;

acmeComponentTypeRef
   : identifier (DOT identifier)?
   ;

acmeComponentInstantiatedTypeRef
   : identifier (DOT identifier)?
   ;

acmeConnectorTypeRef
   : identifier (DOT identifier)?
   ;

acmeConnectorInstantiatedTypeRef
   : identifier (DOT identifier)?
   ;

acmePortTypeRef
   : identifier (DOT identifier)?
   ;

acmePortInstantiatedTypeRef
   : identifier (DOT identifier)?
   ;

acmeGroupTypeRef
   : identifier (DOT identifier)?
   ;

acmeGroupInstantiatedTypeRef
   : identifier (DOT identifier)?
   ;

acmeRoleTypeRef
   : identifier (DOT identifier)?
   ;

acmeRoleInstantiatedTypeRef
   : identifier (DOT identifier)?
   ;

acmeViewTypeRef
   : identifier (DOT identifier)?
   ;

acmeViewInstantiatedTypeRef
   : identifier (DOT identifier)?
   ;

acmeFamilyRef
   : identifier (DOT identifier)?
   ;

acmeFamilyInstantiationRef
   : identifier
   ;

acmeElementTypeRef
   : identifier (DOT identifier)?
   ;

acmePropertyTypeDeclarationRef
   : identifier (DOT identifier)?
   ;

acmeInstanceRef
   : IDENTIFIER (DOT IDENTIFIER)*
   ;

acmeGenericElementTypeDeclaration
   : ELEMENT TYPE identifier (SEMICOLON | ASSIGN acmeGenericElementBody SEMICOLON? | EXTENDS acmeElementTypeRef (COMMA acmeElementTypeRef)* (SEMICOLON | WITH acmeGenericElementBody SEMICOLON?))
   ;

acmeGenericElementBody
   : LBRACE (acmePropertyDeclaration | designRule)* RBRACE
   ;

acmeGroupTypeDeclaration
   : GROUP TYPE identifier (SEMICOLON |  ASSIGN acmeGroupBody SEMICOLON? | EXTENDS acmeGroupTypeRef (COMMA acmeGroupTypeRef)* (SEMICOLON | WITH acmeGroupBody SEMICOLON?))
   ;

acmeGroupDeclaration
   : GROUP identifier (COLON acmeGroupTypeRef (COMMA acmeGroupTypeRef)*)? (SEMICOLON | ASSIGN (acmeGroupBody SEMICOLON? | NEW acmeGroupInstantiatedTypeRef (COMMA acmeGroupInstantiatedTypeRef)* (SEMICOLON | EXTENDED WITH acmeGroupBody SEMICOLON?)))
   ;

acmeGroupBody
   : LBRACE (acmeMembersBlock | acmePropertyDeclaration | designRule)* RBRACE
   ;

acmeMembersBlock
   : MEMBERS LBRACE (acmeInstanceRef (COMMA acmeInstanceRef)*)? RBRACE SEMICOLON?
   ;

acmePortTypeDeclaration
   : PORT TYPE identifier (SEMICOLON |  ASSIGN acmePortBody SEMICOLON? | EXTENDS acmePortTypeRef (COMMA acmePortTypeRef)* (SEMICOLON | WITH acmePortBody SEMICOLON?))
   ;

acmePortDeclaration
   : PORT identifier (COLON acmePortTypeRef (COMMA acmePortTypeRef)*)? (SEMICOLON? | ASSIGN (acmePortBody SEMICOLON? | NEW acmePortInstantiatedTypeRef (COMMA acmePortInstantiatedTypeRef)* (SEMICOLON | EXTENDED WITH acmePortBody SEMICOLON?)))
   ;

acmePortBody
   : LBRACE (acmePropertyDeclaration | designRule | acmeRepresentationDeclaration)* RBRACE
   ;

acmeRoleTypeDeclaration
   : ROLE TYPE identifier (SEMICOLON | ASSIGN acmeRoleBody SEMICOLON? | EXTENDS acmeRoleTypeRef (COMMA acmeRoleTypeRef)* (SEMICOLON | WITH acmeRoleBody SEMICOLON?))
   ;

acmeRoleDeclaration
   : ROLE identifier (COLON acmeRoleTypeRef (COMMA acmeRoleTypeRef)*)? (SEMICOLON | ASSIGN ( acmeRoleBody SEMICOLON? | NEW acmeRoleInstantiatedTypeRef (COMMA acmeRoleInstantiatedTypeRef)* (SEMICOLON | EXTENDED WITH acmeRoleBody SEMICOLON?)))
   ;

acmeRoleBody
   : LBRACE (acmePropertyDeclaration | designRule | acmeRepresentationDeclaration)* RBRACE
   ;

acmeComponentTypeDeclaration
   : COMPONENT TYPE identifier (SEMICOLON | ASSIGN acmeComponentBody SEMICOLON? | EXTENDS acmeComponentTypeRef (COMMA acmeComponentTypeRef)* (SEMICOLON | WITH acmeComponentBody SEMICOLON?))
   ;

acmeComponentDeclaration
   : COMPONENT identifier (COLON acmeComponentTypeRef (COMMA acmeComponentTypeRef)*)? (SEMICOLON | ASSIGN (acmeComponentBody SEMICOLON? | NEW acmeComponentInstantiatedTypeRef (COMMA acmeComponentInstantiatedTypeRef)* (SEMICOLON | EXTENDED WITH acmeComponentBody SEMICOLON?)))
   ;

acmeComponentBody
   : LBRACE (acmePropertyDeclaration | acmePortDeclaration | designRule | acmeRepresentationDeclaration)* RBRACE
   ;

acmeConnectorTypeDeclaration
   : CONNECTOR TYPE identifier (SEMICOLON | ASSIGN acmeConnectorBody SEMICOLON? | EXTENDS acmeConnectorTypeRef (COMMA acmeConnectorTypeRef)* (SEMICOLON | WITH acmeConnectorBody SEMICOLON?))
   ;

acmeConnectorDeclaration
   : CONNECTOR identifier (COLON acmeConnectorTypeRef (COMMA acmeConnectorTypeRef)*)? (SEMICOLON | ASSIGN (acmeConnectorBody SEMICOLON? | NEW acmeConnectorInstantiatedTypeRef (COMMA acmeConnectorInstantiatedTypeRef)* (SEMICOLON | EXTENDED WITH acmeConnectorBody SEMICOLON?)))
   ;

acmeConnectorBody
   : LBRACE (acmePropertyDeclaration | acmeRoleDeclaration | designRule | acmeRepresentationDeclaration)* RBRACE
   ;

acmeRepresentationDeclaration
   : REPRESENTATION (IDENTIFIER ASSIGN)? LBRACE acmeSystemDeclaration acmeBindingsMapDeclaration? RBRACE SEMICOLON?
   ;

acmeBindingsMapDeclaration
   : BINDINGS LBRACE acmeBindingDeclaration* RBRACE SEMICOLON?
   ;

acmeBindingDeclaration
   : acmeInstanceRef TO acmeInstanceRef (LBRACE acmePropertyDeclaration acmePropertyBlock RBRACE)? SEMICOLON
   ;

acmeAttachmentDeclaration
   : ATTACHMENT acmeInstanceRef TO acmeInstanceRef SEMICOLON
   ;

acmePropertyDeclaration
   : PROPERTY identifier (COLON acmePropertyTypeRef)? (ASSIGN acmePropertyValueDeclaration | CONTAINASSIGN acmePropertyValueDeclaration)? acmePropertyBlock? SEMICOLON
   ;

acmePropertyValueDeclaration
   : INTEGER_LITERAL
   | FLOATING_POINT_LITERAL
   | STRING_LITERAL
   | FALSE
   | TRUE
   | acmePropertySet
   | acmePropertyRecord
   | acmePropertySequence
   | enumidentifier
   ;

enumidentifier
   : IDENTIFIER
   ;

acmePropertyElement
   : IDENTIFIER ('.' IDENTIFIER)*
   | acmePropertyCompoundElement
   ;

acmePropertyCompoundElement
   : acmePropertySet
   | acmePropertyRecord
   | acmePropertySequence
   ;

acmePropertySet
   : LBRACE (acmePropertyValueDeclaration (COMMA acmePropertyValueDeclaration)*)? RBRACE
   ;

acmePropertyRecordEntry
   : identifier (COLON acmePropertyTypeRef)? ASSIGN acmePropertyValueDeclaration
   ;

acmePropertyRecord
   : LBRACKET (acmePropertyRecordEntry (SEMICOLON acmePropertyRecordEntry)* SEMICOLON?)? RBRACKET
   ;

acmePropertySequence
   : LANGLE (acmePropertyValueDeclaration (COMMA acmePropertyValueDeclaration)*)? RANGLE
   ;

acmePropertyTypeRecord
   : RECORD LBRACKET acmePropertyRecordFieldDescription* RBRACKET
   ;

acmePropertyTypeSet
   : SET LBRACE acmeTypeRef? RBRACE
   ;

acmePropertyTypeSequence
   : SEQUENCE LANGLE acmePropertyTypeRef? RANGLE
   ;

acmePropertyTypeEnum
   : ENUM LBRACE identifier (COMMA identifier)* RBRACE
   ;

acmePropertyRecordFieldDescription
   : identifier COLON acmePropertyTypeRef SEMICOLON
   ;

acmePropertyTypeRef
   : acmePropertyTypeStructure
   | acmePropertyTypeDeclarationRef
   ;

acmePropertyTypeStructure
   : acmePropertyTypeAny
   | acmePropertyTypeInt
   | acmePropertyTypeFloat
   | acmePropertyTypeDouble
   | acmePropertyTypeString
   | acmePropertyTypeBoolean
   | acmePropertyTypeRecord
   | acmePropertyTypeSet
   | acmePropertyTypeSequence
   | acmePropertyTypeEnum
   ;

acmePropertyTypeDeclaration
   : PROPERTY TYPE identifier (SEMICOLON | ASSIGN (acmePropertyTypeInt | acmePropertyTypeFloat | acmePropertyTypeDouble | acmePropertyTypeString | acmePropertyTypeBoolean | acmePropertyTypeRecord | acmePropertyTypeSet | acmePropertyTypeSequence | acmePropertyTypeEnum | acmePropertyTypeAny) SEMICOLON )
   ;

acmePropertyBlockEntry
   : identifier (COLON acmePropertyTypeRef)? (ASSIGN acmePropertyValueDeclaration | CONTAINASSIGN acmePropertyValueDeclaration)? SEMICOLON
   ;

acmePropertyBlock
   : PROPBEGIN acmePropertyBlockEntry+ PROPEND
   ;

acmePropertyTypeInt
   : INT
   ;

acmePropertyTypeAny
   : ANY
   ;

acmePropertyTypeFloat
   : FLOAT
   ;

acmePropertyTypeDouble
   : DOUBLE
   ;

acmePropertyTypeString
   : STRING
   ;

acmePropertyTypeBoolean
   : BOOLEAN
   ;

acmeViewDeclaration
   : VIEW identifier (COLON acmeViewTypeRef)? (SEMICOLON | ASSIGN (acmeViewBody SEMICOLON? | NEW acmeViewInstantiatedTypeRef (SEMICOLON | EXTENDED WITH acmeViewBody SEMICOLON? )))
   ;

acmeViewTypeDeclaration
   : VIEW TYPE identifier (SEMICOLON | ASSIGN acmeViewBody SEMICOLON? | EXTENDS acmeViewTypeRef (SEMICOLON | WITH acmeViewBody SEMICOLON?) )
   ;

acmeViewBody
   : LBRACE RBRACE
   ;

designRule
   : DESIGN? (RULE IDENTIFIER ASSIGN?)? ( INVARIANT designRuleExpression | HEURISTIC designRuleExpression )? acmePropertyBlock? SEMICOLON
   ;

acmeDesignAnalysisDeclaration
   : DESIGN? ANALYSIS IDENTIFIER LPAREN (formalParam (COMMA formalParam)*)? RPAREN COLON acmeTypeRef ASSIGN designRuleExpression acmePropertyBlock? SEMICOLON | EXTERNAL DESIGN? ANALYSIS IDENTIFIER LPAREN (formalParam (COMMA formalParam)*)? RPAREN COLON acmeTypeRef ASSIGN (codeLiteral | identifier (DOT identifier)*) SEMICOLON
   ;

formalParam
   : identifier COLON acmeTypeRef
   ;

terminatedDesignRuleExpression
   : designRuleExpression SEMICOLON
   ;

designRuleExpression
   : aSTDRImpliesExpression (OR aSTDRImpliesExpression)*
   ;

aSTDRImpliesExpression
   : dRIffExpression (IMPLIES dRIffExpression)*
   ;

dRIffExpression
   : dRAndExpression (IFF dRAndExpression)*
   ;

dRAndExpression
   : dRNegateExpression (AND dRNegateExpression)*
   ;

dRNegateExpression
   : BANG dRNegateExpression | dREqualityExpression
   ;

dREqualityExpression
   : dRRelationalExpression (EQ dRRelationalExpression | NE dRRelationalExpression)*
   ;

dRRelationalExpression
   : dRAdditiveExpression (LANGLE dRAdditiveExpression | RANGLE dRAdditiveExpression | LE dRAdditiveExpression | GE dRAdditiveExpression)*
   ;

dRAdditiveExpression
   : dRMultiplicativeExpression (PLUS dRMultiplicativeExpression | MINUS dRMultiplicativeExpression)*
   ;

dRMultiplicativeExpression
   : dRNegativeExpression (STAR dRNegativeExpression | SLASH dRNegativeExpression | REM dRNegativeExpression | POWER dRNegativeExpression)*
   ;

dRNegativeExpression
   : MINUS dRNegativeExpression
   | primitiveExpression
   ;

primitiveExpression
   : literalConstant
   | reference
   | parentheticalExpression
   | setExpression
   | literalSequence
   | literalRecord
   | quantifiedExpression
   | sequenceExpression
   ;

parentheticalExpression
   : LPAREN designRuleExpression RPAREN
   ;

reference
   : identifier (DOT (identifier | setReference))* actualParams?
   ;

setReference
   : TYPE | COMPONENTS | CONNECTORS | PORTS | ROLES | GROUPS | MEMBERS | PROPERTIES | REPRESENTATIONS | ATTACHEDPORTS | ATTACHEDROLES
   ;

actualParams
   : LPAREN (designRuleExpression (COMMA designRuleExpression)*)? RPAREN
   ;

literalConstant
   : INTEGER_LITERAL | FLOATING_POINT_LITERAL | STRING_LITERAL | TRUE | FALSE | COMPONENT | GROUP | CONNECTOR | PORT | ROLE | SYSTEM | ELEMENT | PROPERTY | INT | FLOAT | DOUBLE | STRING | BOOLEAN | ENUM | SET | SEQUENCE | RECORD
   ;

quantifiedExpression
   : (FORALL | EXISTS UNIQUE?) variableSetDeclaration (COMMA variableSetDeclaration)* BIT_OR designRuleExpression
   ;

distinctVariableSetDeclaration
   : DISTINCT identifier (COMMA identifier)* ((COLON | SET_DECLARE) acmeTypeRef)? IN (setExpression | reference)
   ;

variableSetDeclaration
   : distinctVariableSetDeclaration | identifier (COMMA identifier)* ((COLON | SET_DECLARE) acmeTypeRef)? IN (setExpression | reference)
   ;

sequenceExpression
   : LANGLE pathExpression RANGLE
   ;

setExpression
   : literalSet | setConstructor | pathExpression
   ;

pathExpression
   : SLASH reference ((COLON | SET_DECLARE) acmeTypeRef)? (LBRACKET designRuleExpression RBRACKET)? (SLASH pathExpressionContinuation)*
   ;

pathExpressionContinuation
   : setReference ((COLON | SET_DECLARE) acmeTypeRef)? (LBRACKET designRuleExpression RBRACKET)? (SLASH pathExpressionContinuation)* | ELLIPSIS? reference
   ;

literalSet
   : LBRACE RBRACE | LBRACE (literalConstant | reference) (COMMA (literalConstant | reference))* RBRACE
   ;

literalSequence
   : LANGLE RANGLE | LANGLE (literalConstant | reference) (COMMA (literalConstant | reference))* RANGLE
   ;

literalRecordEntry
   : identifier (COLON acmePropertyTypeRef)? ASSIGN literalConstant
   ;

literalRecord
   : LBRACKET (literalRecordEntry (SEMICOLON literalRecordEntry)* SEMICOLON?)? RBRACKET
   ;

setConstructor
   : LBRACE? SELECT variableSetDeclaration BIT_OR designRuleExpression RBRACE? | LBRACE? COLLECT reference COLON acmeTypeRef IN (setExpression | reference) BIT_OR designRuleExpression RBRACE?
   ;

acmeTypeRef
   : SYSTEM | COMPONENT | GROUP | CONNECTOR | PORT | ROLE | PROPERTY | ELEMENT | TYPE | REPRESENTATION | reference | acmePropertyTypeStructure
   ;

ABSTRACT
   : A B S T R A C T
   ;

ANALYSIS
   : A N A L Y S I S
   ;

AND
   : A N D
   ;

ANY
   : A N Y
   ;

ASSIGN
   : '='
   ;

ATTACHMENT
   : A T T A C H M E N T
   ;

ATTACHMENTS
   : A T T A C H M E N T S
   ;

ATTACHEDPORTS
   : A T T A C H E D P O R T S
   ;

ATTACHEDROLES
   : A T T A C H E D R O L E S
   ;

BANG
   : '!'
   ;

BINDINGS
   : B I N D I N G S
   ;

COLON
   : ':'
   ;

COMMA
   : ','
   ;

COLLECT
   : C O L L E C T
   ;

COMPONENT
   : C O M P O N E N T
   ;

COMPONENTS
   : C O M P O N E N T S
   ;

CONNECTOR
   : C O N N E C T O R
   ;

CONTAINASSIGN
   : C O N T A I N A S S I G N
   ;

CONNECTORS
   : C O N N E C T O R S
   ;

DESIGN
   : D E S I G N
   ;

DISTINCT
   : D I S T I N C T
   ;

DOT
   : '.'
   ;

DOUBLE
   : D O U B L E
   ;

ELEMENT
   : E L E M E N T
   ;

ENUM
   : E N U M
   ;

EXTENDED
   : E X T E N D E D
   ;

EXTENDS
   : E X T E N D S
   ;

EXTERNAL
   : E X T E R N A L
   ;

EXISTS
   : E X I S T S
   ;

ELLIPSIS
   : '...'
   ;

EQ
   : '=='
   ;

FAMILY
   : F A M I L Y
   ;

FINAL
   : F I N A L
   ;

FORALL
   : F O R A L L
   ;

FLOAT
   : F L O A T
   ;

GROUP
   : G R O U P
   ;

GROUPS
   : G R O U P S
   ;

GE
   : '>='
   ;

HEURISTIC
   : H E U R I S T I C
   ;

IFF
   : '<->'
   ;

IMPORT
   : I M P O R T
   ;

IN
   : I N
   ;

INT
   : I N T
   | I N T E G E R
   ;

INVARIANT
   : I N V A R I A N T
   ;

IMPLIES
   : '->'
   ;

LBRACE
   : '{'
   ;

RBRACE
   : '}'
   ;

LBRACKET
   : '['
   ;

RBRACKET
   : ']'
   ;

LPAREN
   : '('
   ;

RPAREN
   : ')'
   ;

LANGLE
   : '<'
   ;

RANGLE
   : '>'
   ;

LE
   : '<='
   ;

NE
   : '!='
   ;

NEW
   : N E W
   ;

MEMBERS
   : M E M B E R S
   ;

MINUS
   : '-'
   ;

OR
   : O R
   ;

PATHSEPARATOR
   : '.' | ':' | '-' | '+' | '\\' | '\\\\' | '/' | '$' | '%'
   ;

PUBLIC
   : P U B L I C
   ;

PRIVATE
   : P R I V A T E
   ;

POWER
   : P O W E R
   ;

PLUS
   : '+'
   ;

PORT
   : P O R T
   ;

PORTS
   : P O R T S
   ;

PROPERTY
   : P R O P E R T Y
   ;

PROPERTIES
   : P R O P E R T I E S
   ;

PROPBEGIN
   : '<<'
   ;

PROPEND
   : '>>'
   ;

RECORD
   : R E C O R D
   ;

REPRESENTATION
   : R E P R E S E N T A T I O N
   ;

REM
   : '%'
   ;

REPRESENTATIONS
   : R E P R E S E N T A T I O N S
   ;

ROLE
   : R O L E
   ;

RULE
   : R U L E
   ;

ROLES
   : R O L E S
   ;

SEQUENCE
   : S E Q U E N C E
   | S E Q
   ;

SELECT
   : S E L E C T
   ;

SEMICOLON
   : ';'
   ;

SET
   : S E T
   ;

SET_DECLARE
   : ':!'
   ;

SLASH
   : '/'
   ;

STAR
   : '*'
   ;

STRING
   : S T R I N G
   ;

STYLE
   : S T Y L E
   ;

SYSTEM
   : S Y S T E M
   ;

TO
   : T O
   ;

TYPE
   : T Y P E
   ;

UNIQUE
   : U N I Q U E
   ;

WITH
   : W I T H
   ;

VIEW
   : V I E W
   ;

BIT_OR
   : '|'
   ;

TRUE
   : T R U E
   ;

FALSE
   : F A L S E
   ;

fragment A
   : 'a' | 'A'
   ;

fragment B
   : 'b' | 'B'
   ;

fragment C
   : 'c' | 'C'
   ;

fragment D
   : 'd' | 'D'
   ;

fragment E
   : 'e' | 'E'
   ;

fragment F
   : 'f' | 'F'
   ;

fragment G
   : 'g' | 'G'
   ;

fragment H
   : 'h' | 'H'
   ;

fragment I
   : 'i' | 'I'
   ;

fragment J
   : 'j' | 'J'
   ;

fragment K
   : 'k' | 'K'
   ;

fragment L
   : 'l' | 'L'
   ;

fragment M
   : 'm' | 'M'
   ;

fragment N
   : 'n' | 'N'
   ;

fragment O
   : 'o' | 'O'
   ;

fragment P
   : 'p' | 'P'
   ;

fragment Q
   : 'q' | 'Q'
   ;

fragment R
   : 'r' | 'R'
   ;

fragment S
   : 's' | 'S'
   ;

fragment T
   : 't' | 'T'
   ;

fragment U
   : 'u' | 'U'
   ;

fragment V
   : 'v' | 'V'
   ;

fragment W
   : 'w' | 'W'
   ;

fragment X
   : 'x' | 'X'
   ;

fragment Y
   : 'y' | 'Y'
   ;

fragment Z
   : 'z' | 'Z'
   ;

BOOLEAN
   : TRUE
   | FALSE
   ;

FLOATING_POINT_LITERAL
   : ('-' | '+')? [0-9]+ '.' [0-9]+
   ;

INTEGER_LITERAL
   : [0-9]+
   ;

STRING_LITERAL
   : '"' .*? '"'
   ;

IDENTIFIER
   : [a-zA-Z] [a-zA-Z0-9_-]*
   ;

LINE_COMMENT
   : '//' ~ [\r\n]* -> skip
   ;

BLOCK_COMMENT
   : '/*' .*? '*/' -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

