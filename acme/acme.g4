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

// $antlr-format alignTrailingComments on, columnLimit 250, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments off
// $antlr-format useTab off, allowShortRulesOnASingleLine off, allowShortBlocksOnASingleLine on, alignSemicolons hanging
// $antlr-format alignColons hanging

grammar acme;

options { caseInsensitive=true; }

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
    : (FAMILY | STYLE) identifier (
        SEMICOLON
        | ASSIGN acmeFamilyBody SEMICOLON?
        | EXTENDS acmeFamilyRef (COMMA acmeFamilyRef)* (SEMICOLON | WITH acmeFamilyBody SEMICOLON?)
    )
    ;

acmeFamilyBody
    : LBRACE (
        (PUBLIC | PRIVATE)? (FINAL | ABSTRACT)? (
            acmePortTypeDeclaration
            | acmeRoleTypeDeclaration
            | acmeComponentTypeDeclaration
            | acmeConnectorTypeDeclaration
            | acmeGenericElementTypeDeclaration
            | acmePropertyTypeDeclaration
            | acmeGroupTypeDeclaration
        )
        | acmeDesignAnalysisDeclaration
        | designRule
        | acmePortDeclaration
        | acmeRoleDeclaration
        | acmeComponentDeclaration
        | acmeConnectorDeclaration
        | acmePropertyDeclaration
        | acmeGroupDeclaration
        | acmeAttachmentDeclaration
    )* RBRACE
    ;

acmeSystemDeclaration
    : SYSTEM identifier (COLON acmeFamilyRef (COMMA acmeFamilyRef)*)? (
        SEMICOLON
        | ASSIGN (
            acmeSystemBody SEMICOLON?
            | NEW acmeFamilyInstantiationRef (COMMA acmeFamilyInstantiationRef)* ( SEMICOLON | EXTENDED WITH acmeSystemBody SEMICOLON?)
        )
    )
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
    : ELEMENT TYPE identifier (
        SEMICOLON
        | ASSIGN acmeGenericElementBody SEMICOLON?
        | EXTENDS acmeElementTypeRef (COMMA acmeElementTypeRef)* ( SEMICOLON | WITH acmeGenericElementBody SEMICOLON?)
    )
    ;

acmeGenericElementBody
    : LBRACE (acmePropertyDeclaration | designRule)* RBRACE
    ;

acmeGroupTypeDeclaration
    : GROUP TYPE identifier (
        SEMICOLON
        | ASSIGN acmeGroupBody SEMICOLON?
        | EXTENDS acmeGroupTypeRef (COMMA acmeGroupTypeRef)* ( SEMICOLON | WITH acmeGroupBody SEMICOLON?)
    )
    ;

acmeGroupDeclaration
    : GROUP identifier (COLON acmeGroupTypeRef (COMMA acmeGroupTypeRef)*)? (
        SEMICOLON
        | ASSIGN (
            acmeGroupBody SEMICOLON?
            | NEW acmeGroupInstantiatedTypeRef (COMMA acmeGroupInstantiatedTypeRef)* ( SEMICOLON | EXTENDED WITH acmeGroupBody SEMICOLON?)
        )
    )
    ;

acmeGroupBody
    : LBRACE (acmeMembersBlock | acmePropertyDeclaration | designRule)* RBRACE
    ;

acmeMembersBlock
    : MEMBERS LBRACE (acmeInstanceRef (COMMA acmeInstanceRef)*)? RBRACE SEMICOLON?
    ;

acmePortTypeDeclaration
    : PORT TYPE identifier (SEMICOLON | ASSIGN acmePortBody SEMICOLON? | EXTENDS acmePortTypeRef (COMMA acmePortTypeRef)* ( SEMICOLON | WITH acmePortBody SEMICOLON?))
    ;

acmePortDeclaration
    : PORT identifier (COLON acmePortTypeRef (COMMA acmePortTypeRef)*)? (
        SEMICOLON?
        | ASSIGN (acmePortBody SEMICOLON? | NEW acmePortInstantiatedTypeRef (COMMA acmePortInstantiatedTypeRef)* ( SEMICOLON | EXTENDED WITH acmePortBody SEMICOLON?))
    )
    ;

acmePortBody
    : LBRACE (acmePropertyDeclaration | designRule | acmeRepresentationDeclaration)* RBRACE
    ;

acmeRoleTypeDeclaration
    : ROLE TYPE identifier (SEMICOLON | ASSIGN acmeRoleBody SEMICOLON? | EXTENDS acmeRoleTypeRef (COMMA acmeRoleTypeRef)* ( SEMICOLON | WITH acmeRoleBody SEMICOLON?))
    ;

acmeRoleDeclaration
    : ROLE identifier (COLON acmeRoleTypeRef (COMMA acmeRoleTypeRef)*)? (
        SEMICOLON
        | ASSIGN (acmeRoleBody SEMICOLON? | NEW acmeRoleInstantiatedTypeRef (COMMA acmeRoleInstantiatedTypeRef)* ( SEMICOLON | EXTENDED WITH acmeRoleBody SEMICOLON?))
    )
    ;

acmeRoleBody
    : LBRACE (acmePropertyDeclaration | designRule | acmeRepresentationDeclaration)* RBRACE
    ;

acmeComponentTypeDeclaration
    : COMPONENT TYPE identifier (
        SEMICOLON
        | ASSIGN acmeComponentBody SEMICOLON?
        | EXTENDS acmeComponentTypeRef (COMMA acmeComponentTypeRef)* ( SEMICOLON | WITH acmeComponentBody SEMICOLON?)
    )
    ;

acmeComponentDeclaration
    : COMPONENT identifier (COLON acmeComponentTypeRef (COMMA acmeComponentTypeRef)*)? (
        SEMICOLON
        | ASSIGN (
            acmeComponentBody SEMICOLON?
            | NEW acmeComponentInstantiatedTypeRef (COMMA acmeComponentInstantiatedTypeRef)* (SEMICOLON | EXTENDED WITH acmeComponentBody SEMICOLON?)
        )
    )
    ;

acmeComponentBody
    : LBRACE (acmePropertyDeclaration | acmePortDeclaration | designRule | acmeRepresentationDeclaration)* RBRACE
    ;

acmeConnectorTypeDeclaration
    : CONNECTOR TYPE identifier (
        SEMICOLON
        | ASSIGN acmeConnectorBody SEMICOLON?
        | EXTENDS acmeConnectorTypeRef (COMMA acmeConnectorTypeRef)* ( SEMICOLON | WITH acmeConnectorBody SEMICOLON?)
    )
    ;

acmeConnectorDeclaration
    : CONNECTOR identifier (COLON acmeConnectorTypeRef (COMMA acmeConnectorTypeRef)*)? (
        SEMICOLON
        | ASSIGN (
            acmeConnectorBody SEMICOLON?
            | NEW acmeConnectorInstantiatedTypeRef (COMMA acmeConnectorInstantiatedTypeRef)* (SEMICOLON | EXTENDED WITH acmeConnectorBody SEMICOLON?)
        )
    )
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
    | (MINUS | PLUS)* FLOATING_POINT_LITERAL
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
    : LBRACE (acmePropertyValueDeclaration ( COMMA acmePropertyValueDeclaration)*)? RBRACE
    ;

acmePropertyRecordEntry
    : identifier (COLON acmePropertyTypeRef)? ASSIGN acmePropertyValueDeclaration
    ;

acmePropertyRecord
    : LBRACKET (acmePropertyRecordEntry (SEMICOLON acmePropertyRecordEntry)* SEMICOLON?)? RBRACKET
    ;

acmePropertySequence
    : LANGLE (acmePropertyValueDeclaration ( COMMA acmePropertyValueDeclaration)*)? RANGLE
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
    : PROPERTY TYPE identifier (
        SEMICOLON
        | ASSIGN (
            acmePropertyTypeInt
            | acmePropertyTypeFloat
            | acmePropertyTypeDouble
            | acmePropertyTypeString
            | acmePropertyTypeBoolean
            | acmePropertyTypeRecord
            | acmePropertyTypeSet
            | acmePropertyTypeSequence
            | acmePropertyTypeEnum
            | acmePropertyTypeAny
        ) SEMICOLON
    )
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
    : boolean
    ;

acmeViewDeclaration
    : VIEW identifier (COLON acmeViewTypeRef)? (
        SEMICOLON
        | ASSIGN ( acmeViewBody SEMICOLON? | NEW acmeViewInstantiatedTypeRef (SEMICOLON | EXTENDED WITH acmeViewBody SEMICOLON?))
    )
    ;

acmeViewTypeDeclaration
    : VIEW TYPE identifier (SEMICOLON | ASSIGN acmeViewBody SEMICOLON? | EXTENDS acmeViewTypeRef (SEMICOLON | WITH acmeViewBody SEMICOLON?))
    ;

acmeViewBody
    : LBRACE RBRACE
    ;

designRule
    : DESIGN? (RULE IDENTIFIER ASSIGN?)? (INVARIANT designRuleExpression | HEURISTIC designRuleExpression)? acmePropertyBlock? SEMICOLON
    ;

acmeDesignAnalysisDeclaration
    : DESIGN? ANALYSIS IDENTIFIER LPAREN (formalParam (COMMA formalParam)*)? RPAREN COLON acmeTypeRef ASSIGN designRuleExpression acmePropertyBlock? SEMICOLON
    | EXTERNAL DESIGN? ANALYSIS IDENTIFIER LPAREN (formalParam (COMMA formalParam)*)? RPAREN COLON acmeTypeRef ASSIGN (codeLiteral | identifier (DOT identifier)*) SEMICOLON
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
    : BANG dRNegateExpression
    | dREqualityExpression
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
    : TYPE
    | COMPONENTS
    | CONNECTORS
    | PORTS
    | ROLES
    | GROUPS
    | MEMBERS
    | PROPERTIES
    | REPRESENTATIONS
    | ATTACHEDPORTS
    | ATTACHEDROLES
    ;

actualParams
    : LPAREN (designRuleExpression (COMMA designRuleExpression)*)? RPAREN
    ;

literalConstant
    : INTEGER_LITERAL
    | FLOATING_POINT_LITERAL
    | STRING_LITERAL
    | TRUE
    | FALSE
    | COMPONENT
    | GROUP
    | CONNECTOR
    | PORT
    | ROLE
    | SYSTEM
    | ELEMENT
    | PROPERTY
    | INT
    | FLOAT
    | DOUBLE
    | STRING
    | boolean
    | ENUM
    | SET
    | SEQUENCE
    | RECORD
    ;

quantifiedExpression
    : (FORALL | EXISTS UNIQUE?) variableSetDeclaration (COMMA variableSetDeclaration)* BIT_OR designRuleExpression
    ;

distinctVariableSetDeclaration
    : DISTINCT identifier (COMMA identifier)* ((COLON | SET_DECLARE) acmeTypeRef)? IN (setExpression | reference)
    ;

variableSetDeclaration
    : distinctVariableSetDeclaration
    | identifier (COMMA identifier)* ((COLON | SET_DECLARE) acmeTypeRef)? IN ( setExpression | reference)
    ;

sequenceExpression
    : LANGLE pathExpression RANGLE
    ;

setExpression
    : literalSet
    | setConstructor
    | pathExpression
    ;

pathExpression
    : SLASH reference ((COLON | SET_DECLARE) acmeTypeRef)? (LBRACKET designRuleExpression RBRACKET)? (SLASH pathExpressionContinuation)*
    ;

pathExpressionContinuation
    : setReference ((COLON | SET_DECLARE) acmeTypeRef)? (LBRACKET designRuleExpression RBRACKET)? (SLASH pathExpressionContinuation)*
    | ELLIPSIS? reference
    ;

literalSet
    : LBRACE RBRACE
    | LBRACE (literalConstant | reference) (COMMA (literalConstant | reference))* RBRACE
    ;

literalSequence
    : LANGLE RANGLE
    | LANGLE (literalConstant | reference) (COMMA (literalConstant | reference))* RANGLE
    ;

literalRecordEntry
    : identifier (COLON acmePropertyTypeRef)? ASSIGN literalConstant
    ;

literalRecord
    : LBRACKET (literalRecordEntry (SEMICOLON literalRecordEntry)* SEMICOLON?)? RBRACKET
    ;

setConstructor
    : LBRACE? SELECT variableSetDeclaration BIT_OR designRuleExpression RBRACE?
    | LBRACE? COLLECT reference COLON acmeTypeRef IN (setExpression | reference) BIT_OR designRuleExpression RBRACE?
    ;

acmeTypeRef
    : SYSTEM
    | COMPONENT
    | GROUP
    | CONNECTOR
    | PORT
    | ROLE
    | PROPERTY
    | ELEMENT
    | TYPE
    | REPRESENTATION
    | reference
    | acmePropertyTypeStructure
    ;

ABSTRACT
    : 'abstract'
    ;

ANALYSIS
    : 'analysis'
    ;

AND
    : 'and'
    ;

ANY
    : 'any'
    ;

ASSIGN
    : '='
    ;

ATTACHMENT
    : 'attachment'
    ;

ATTACHMENTS
    : 'attachments'
    ;

ATTACHEDPORTS
    : 'attachedports'
    ;

ATTACHEDROLES
    : 'attachedroles'
    ;

BANG
    : '!'
    ;

BINDINGS
    : 'bindings'
    ;

COLON
    : ':'
    ;

COMMA
    : ','
    ;

COLLECT
    : 'collect'
    ;

COMPONENT
    : 'component'
    ;

COMPONENTS
    : 'components'
    ;

CONNECTOR
    : 'connector'
    ;

CONTAINASSIGN
    : 'containassign'
    ;

CONNECTORS
    : 'connectors'
    ;

DESIGN
    : 'design'
    ;

DISTINCT
    : 'distinct'
    ;

DOT
    : '.'
    ;

DOUBLE
    : 'double'
    ;

ELEMENT
    : 'element'
    ;

ENUM
    : 'enum'
    ;

EXTENDED
    : 'extended'
    ;

EXTENDS
    : 'extends'
    ;

EXTERNAL
    : 'external'
    ;

EXISTS
    : 'exists'
    ;

ELLIPSIS
    : '...'
    ;

EQ
    : '=='
    ;

FAMILY
    : 'family'
    ;

FINAL
    : 'final'
    ;

FORALL
    : 'forall'
    ;

FLOAT
    : 'float'
    ;

GROUP
    : 'group'
    ;

GROUPS
    : 'groups'
    ;

GE
    : '>='
    ;

HEURISTIC
    : 'heuristic'
    ;

IFF
    : '<->'
    ;

IMPORT
    : 'import'
    ;

IN
    : 'in'
    ;

INT
    : 'int'
    | 'integer'
    ;

INVARIANT
    : 'invariant'
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
    : 'new'
    ;

MEMBERS
    : 'members'
    ;

MINUS
    : '-'
    ;

OR
    : 'or'
    ;

pathseparator
    : '.'
    | ':'
    | '-'
    | '+'
    | '\\'
    | '\\\\'
    | '/'
    | '$'
    | '%'
    ;

PUBLIC
    : 'public'
    ;

PRIVATE
    : 'private'
    ;

POWER
    : 'power'
    ;

PLUS
    : '+'
    ;

PORT
    : 'port'
    ;

PORTS
    : 'ports'
    ;

PROPERTY
    : 'property'
    ;

PROPERTIES
    : 'properties'
    ;

PROPBEGIN
    : '<<'
    ;

PROPEND
    : '>>'
    ;

RECORD
    : 'record'
    ;

REPRESENTATION
    : 'representation'
    ;

REM
    : '%'
    ;

REPRESENTATIONS
    : 'representations'
    ;

ROLE
    : 'role'
    ;

RULE
    : 'rule'
    ;

ROLES
    : 'roles'
    ;

SEQUENCE
    : 'sequence'
    | 'seq'
    ;

SELECT
    : 'select'
    ;

SEMICOLON
    : ';'
    ;

SET
    : 'set'
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
    : 'string'
    ;

STYLE
    : 'style'
    ;

SYSTEM
    : 'system'
    ;

TO
    : 'to'
    ;

TYPE
    : 'type'
    ;

UNIQUE
    : 'unique'
    ;

WITH
    : 'with'
    ;

VIEW
    : 'view'
    ;

BIT_OR
    : '|'
    ;

TRUE
    : 'true'
    ;

FALSE
    : 'false'
    ;

boolean
    : TRUE
    | FALSE
    ;

FLOATING_POINT_LITERAL
    : [0-9]+ '.' [0-9]+
    ;

INTEGER_LITERAL
    : [0-9]+
    ;

STRING_LITERAL
    : '"' .*? '"'
    ;

IDENTIFIER
    : [a-z] [a-z0-9_-]*
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