// PHP grammar by Ivan Kochurkin (KvanTTT), 2015.
// Used Phalanger grammar: https://github.com/DEVSENSE/Phalanger by Jakub Míšek (jakubmisek)
// and old php grammar by Tom Everett (teverett).
// Runtime: C#.
// Licence: MIT.

parser grammar PHPParser;

options { tokenVocab=PHPLexer; }

// HTML
// Also see here: https://github.com/antlr/grammars-v4/tree/master/html

htmlDocument
    : Shebang? HtmlComment* htmlElementOrPhpBlock* EOF
    ;

htmlElementOrPhpBlock
    : (HtmlDtd | htmlElement | phpBlock) HtmlComment*
    ;

htmlElement
    : HtmlScriptOpen htmlAttribute* HtmlClose scriptText ScriptClose
    | HtmlStyleOpen htmlAttribute* HtmlClose StyleBody
    | HtmlOpen HtmlName htmlAttribute* HtmlClose (htmlContent HtmlOpen HtmlSlash HtmlName HtmlClose)?
    | HtmlOpen HtmlName htmlAttribute* '/>'
    ;

htmlContent
    : HtmlText? ((htmlElement | HtmlComment | phpBlock) HtmlText?)*
    ;

htmlAttribute
    : phpBlock
    | HtmlName HtmlEquals HtmlStartQuoteString htmlQuotePhpBlockOrString* HtmlEndQuoteString
    | HtmlName HtmlEquals HtmlStartDoubleQuoteString htmlDoubleQuotePhpBlockOrString* HtmlEndDoubleQuoteString
    | HtmlName HtmlEquals (HtmlHex | HtmlDecimal)
    | HtmlName
    ;

htmlQuotePhpBlockOrString
    : phpBlock
    | HtmlQuoteString
    ;

htmlDoubleQuotePhpBlockOrString
    : phpBlock
    | HtmlDoubleQuoteString
    ;
    
// Script
// Parse JavaScript with https://github.com/antlr/grammars-v4/tree/master/ecmascript if necessary.

scriptText
    : scriptTextPart*
    ;
    
scriptTextPart
    : phpBlock
    | ScriptText+
    ;

// PHP
    
phpBlock
    : importStatement* topStatement+
    ;

importStatement
    : Import Namespace namespaceNameList ';'
    ;

topStatement
    : emptyStatement
    | nonEmptyStatement
    | useDeclaration
    | namespaceDeclaration
    | functionDeclaration
    | classDeclaration
    | globalConstantDeclaration
    ;
    
useDeclaration
    : Use (Function | Const)? useDeclarationContentList ';'
    ;

useDeclarationContentList
    : '\\'? useDeclarationContent (',' '\\'? useDeclarationContent)*
    ;
    
useDeclarationContent
    : namespaceNameList (As identifier)?
    ;

namespaceDeclaration
    : Namespace (namespaceNameList? '{' namespaceStatement* '}' | namespaceNameList ';')
    ;

namespaceStatement
    : nonEmptyStatement
    | useDeclaration
    | functionDeclaration
    | classDeclaration
    | globalConstantDeclaration
    ;

functionDeclaration
    : attributes Function '&'? identifier typeParameterListInBrackets? '(' formalParameterList ')' blockStatement
    ;

classDeclaration
    : attributes Private? modifier? Partial? (
      classEntryType identifier typeParameterListInBrackets? (Extends qualifiedStaticTypeRef)? (Implements interfaceList)? 
    | Interface identifier typeParameterListInBrackets? (Extends interfaceList)? )
      '{' classStatement* '}'
    ;
    
classEntryType
    : Class
    | Trait
    ;

interfaceList
    : qualifiedStaticTypeRef (',' qualifiedStaticTypeRef)*
    ;

typeParameterListInBrackets
    : '<:' typeParameterList ':>'
    | '<:' typeParameterWithDefaultsList ':>'
    | '<:' typeParameterList ',' typeParameterWithDefaultsList ':>'
    ;

typeParameterList
    : typeParameterDecl (',' typeParameterDecl)*
    ;

typeParameterWithDefaultsList
    : typeParameterWithDefaultDecl (',' typeParameterWithDefaultDecl)*
    ;

typeParameterDecl
    : attributes identifier
    ;

typeParameterWithDefaultDecl
    : attributes identifier Eq (qualifiedStaticTypeRef | primitiveType)
    ;

genericDynamicArgs
    : '<:' typeRef (',' typeRef)* ':>'
    ;

attributes
    : attributesGroup*
    ;

attributesGroup
    : '[' (identifier ':')? attribute (',' attribute)* ']'
    ;

attribute
    : qualifiedNamespaceName
    | qualifiedNamespaceName '(' attributeArgList ')'
    | qualifiedNamespaceName '(' attributeNamedArgList ')'
    | qualifiedNamespaceName '(' attributeArgList ',' attributeNamedArgList ')'
    ;

attributeArgList
    : expression (',' expression)*
    ;

attributeNamedArgList
    : attributeNamedArg (',' attributeNamedArg)*
    ;

attributeNamedArg
    : VarName '=>' expression
    ;

innerStatementList
    : innerStatement*
    ;

innerStatement
    : statement
    | functionDeclaration
    | classDeclaration
    ;

// Statements
    
statement
    : nonEmptyStatement
    | emptyStatement
    ;

emptyStatement
    : ';'
    ;

nonEmptyStatement
    : identifier ':'
    | blockStatement
    | ifStatement
    | whileStatement
    | doWhileStatement
    | forStatement
    | switchStatement
    | breakStatement
    | continueStatement
    | returnStatement
    | yieldExpression ';'
    | globalStatement
    | staticVariableStatement
    | echoStatement
    | expressionStatement
    | unsetStatement
    | foreachStatement
    | tryCatchFinally
    | throwStatement
    | gotoStatement
    | declareStatement
    | inlineHtml
    ;

blockStatement
    : '{' innerStatementList '}'
    ;
    
ifStatement
    : If parenthesis statement elseIfStatement* elseStatement?
    | If parenthesis ':' innerStatementList elseIfColonStatement* elseColonStatement? EndIf ';'
    ;

elseIfStatement
    : ElseIf parenthesis statement
    ;

elseIfColonStatement
    : ElseIf parenthesis ':' innerStatementList
    ;

elseStatement
    : Else statement
    ;

elseColonStatement
    : Else ':' innerStatementList
    ;

whileStatement
    : While parenthesis (statement | ':' innerStatementList EndWhile ';')
    ;

doWhileStatement
    : Do statement While parenthesis ';'
    ;
    
forStatement
    : For '(' forInit? ';' expressionList? ';' forUpdate? ')' (statement | ':' innerStatementList EndFor ';' )
    ;

forInit
    : expressionList
    ;
    
forUpdate
    : expressionList
    ;
    
switchStatement
    : Switch parenthesis ('{' ';'? switchBlock* '}' | ':' ';'? switchBlock* EndSwitch ';')
    ;

switchBlock
    : ((Case expression | Default) ( ':' | ';' ))+ innerStatementList
    ;
    
breakStatement
    : Break expression? ';'
    ;
    
continueStatement
    : Continue expression? ';'
    ;
    
returnStatement
    : Return expression? ';'
    ;

expressionStatement
    : expression ';'
    ;

unsetStatement
    : Unset '(' chainList ')' ';'
    ;
    
foreachStatement
    : Foreach 
        ( '(' chain As '&'? chain ('=>' '&'? chain)? ')'
        | '(' expression As chain ('=>' '&'? chain)? ')'
        | '(' chain As List '(' assignmentList ')' ')' )
      (statement | ':' innerStatementList EndForeach ';')
    ;
    
tryCatchFinally
    : Try blockStatement (catchClause+ finallyStatement? | catchClause* finallyStatement)
    ;

catchClause
    : Catch '(' qualifiedStaticTypeRef VarName ')' blockStatement
    ;

finallyStatement
    : Finally blockStatement
    ;
    
throwStatement
    : Throw expression ';'
    ;
    
gotoStatement
    : Goto identifier ';'
    ;

declareStatement
    : Declare '(' declareList ')' (statement | ':' innerStatementList EndDeclare ';')
    ;

inlineHtml
    : HtmlComment* ((HtmlDtd | htmlElement) HtmlComment*)+
    ;

declareList
    : identifierInititalizer (',' identifierInititalizer)*
    ;

formalParameterList
    : formalParameter? (',' formalParameter)*
    ;

formalParameter
    : attributes typeHint? '&'? '...'? variableInitializer
    ;

typeHint
    : qualifiedStaticTypeRef
    | Callable
    | primitiveType
    ;

globalStatement
    : Global globalVar (',' globalVar)* ';'
    ;

globalVar
    : VarName
    | '$' chain
    | '$' '{' expression '}'
    ;

echoStatement
    : Echo expressionList ';'
    ;

staticVariableStatement
    : Static variableInitializer (',' variableInitializer)* ';'
    ;

classStatement
    : attributes propertyModifiers variableInitializer (',' variableInitializer)* ';'
    | attributes Const identifierInititalizer (',' identifierInititalizer)* ';'
    | attributes memberModifiers? Function '&'? identifier
          typeParameterListInBrackets? '(' formalParameterList ')' baseCtorCall? methodBody
    | Use qualifiedNamespaceNameList traitAdaptations
    ;

traitAdaptations
    : ';'
    | '{' traitAdaptationStatement* '}'
    ;

traitAdaptationStatement
    : traitPrecedence
    | traitAlias
    ;

traitPrecedence
    : qualifiedNamespaceName '::' identifier InsteadOf qualifiedNamespaceNameList ';'
    ;
    
traitAlias
    : traitMethodReference As (memberModifier | memberModifier? identifier) ';'
    ;
    
traitMethodReference
    : (qualifiedNamespaceName '::')? identifier
    ;

baseCtorCall
    : ':' identifier arguments
    ;

methodBody
    : ';'
    | blockStatement
    ;

propertyModifiers
    : memberModifiers
    | Var
    ;

memberModifiers
    : memberModifier+
    ;

variableInitializer
    : VarName (Eq constantInititalizer)?
    ;

identifierInititalizer
    : identifier Eq constantInititalizer
    ;

globalConstantDeclaration
    : attributes Const identifierInititalizer (',' identifierInititalizer)* ';'
    ;

expressionList
    : expression (',' expression)*
    ;

parenthesis
    : '(' (expression | yieldExpression) ')'
    ;

// Expressions

expression
// Grouped by prioriries: http://php.net/manual/en/language.operators.precedence.php
// and http://www.phpeveryday.com/articles/PHP-Operators-Operator-Priority-P312.html
    : Clone expression                                         #CloneExpression
    | newExpr                                                     #NewExpression
    
    | stringConstant '[' expression ']'                        #IndexerExpression

    | <assoc=right> expression '**' expression                 #BinaryOperatorExpression

    | ('++' | '--') chain                                      #PrefixIncDecExpression
    | chain ('++' | '--')                                      #PostfixIncDecExpression
    
    | '(' castOperation ')' expression                         #CastExpression
    | ('~' | '@') expression                                   #UnaryOperatorExpression

    | expression InstanceOf typeRef                            #InstanceOfExpression
    
    | ('!' | '+' | '-') expression                             #UnaryOperatorExpression
    
    | expression ('*' | Divide | '%') expression               #BinaryOperatorExpression

    | expression ('+' | '-' | '.') expression                  #BinaryOperatorExpression

    | expression ('<<' | '>>') expression                      #BinaryOperatorExpression

    | expression (Less | '<=' | Greater | '>=') expression     #BinaryOperatorExpression

    | expression ('===' | '!==' | '==' | IsNotEq) expression   #BinaryOperatorExpression

    | expression '&' expression                                #BinaryOperatorExpression

    | expression '^' expression                                #BinaryOperatorExpression

    | expression '|' expression                                #BinaryOperatorExpression

    | expression '&&' expression                               #BinaryOperatorExpression

    | expression '||' expression                               #BinaryOperatorExpression

    | expression QuestionMark expression? ':' expression       #ConditionalExpression
    
    | chain assignmentOperator expression                      #AssignmentExpression
    | chain Eq '&' (chain | newExpr)                           #AssignmentExpression

    | Print expression                                         #PrintExpression
    
    | expression LogicalAnd  expression                        #BinaryOperatorExpression
    
    | expression LogicalXor expression                         #BinaryOperatorExpression
    
    | expression LogicalOr  expression                         #BinaryOperatorExpression

    | chain                                                    #ChainExpression
    | constant                                                 #ScalarExpression
    | string                                                   #ScalarExpression
    | Label                                                    #ScalarExpression

    | BackQuoteString                                          #BackQuoteStringExpression
    | parenthesis                                              #ParenthesisExpression
    | (Array '(' arrayItemList? ')' | '[' arrayItemList? ']') ('[' expression ']')?            #ArrayCreationExpression

    | Yield                                                    #SpecialWordExpression
    | List '(' assignmentList ')' Eq expression                #SpecialWordExpression
    | IsSet '(' chainList ')'                                  #SpecialWordExpression
    | Empty '(' chain ')'                                      #SpecialWordExpression
    | Eval '(' expression ')'                                  #SpecialWordExpression
    | Exit ( '(' ')' | parenthesis )?                          #SpecialWordExpression
    | Include expression                                       #SpecialWordExpression
    | IncludeOnce expression                                   #SpecialWordExpression
    | Require expression                                       #SpecialWordExpression
    | RequireOnce expression                                   #SpecialWordExpression

    | Static? Function '&'? '(' formalParameterList ')' lambdaFunctionUseVars? blockStatement  #LambdaFunctionExpression
    ;

newExpr
    : New typeRef arguments?
    ;

assignmentOperator
    : Eq
    | '+='
    | '-='
    | '*='
    | '**='
    | '/='
    | '.='
    | '%='
    | '&='
    | '|='
    | '^='
    | '<<='
    | '>>='
    ;

yieldExpression
    : Yield expression ('=>' expression)?
    ;

arrayItemList
    : arrayItem (',' arrayItem)* ','?
    ;

arrayItem
    : expression ('=>' expression)?
    | (expression '=>')? '&' chain
    ;

lambdaFunctionUseVars
    : Use '(' lambdaFunctionUseVar (',' lambdaFunctionUseVar)* ')'
    ;

lambdaFunctionUseVar
    : '&'? VarName
    ;

qualifiedStaticTypeRef
    : qualifiedNamespaceName genericDynamicArgs?
    | Static
    ;

typeRef
    : (qualifiedNamespaceName | indirectTypeRef) genericDynamicArgs?
    | primitiveType
    | Static
    ;

indirectTypeRef
    : chainBase ('->' keyedFieldName)*
    ;

qualifiedNamespaceName
    : Namespace? '\\'? namespaceNameList
    ;

namespaceNameList
    : identifier ('\\' identifier)*
    ;

qualifiedNamespaceNameList
    : qualifiedNamespaceName (',' qualifiedNamespaceName)*
    ;

arguments
    : '(' ( actualArgument (',' actualArgument)* | yieldExpression)? ')'
    ;
    
actualArgument
    : '...'? expression
    | '&' chain
    ;

constantInititalizer
    : constant
    | string
    | Array '(' (constantArrayItemList ','?)? ')'
    | '[' (constantArrayItemList ','?)? ']'
    | ('+'|'-') constantInititalizer
    ;
    
constantArrayItemList
    : constantArrayItem (',' constantArrayItem)*
    ;

constantArrayItem
    : constantInititalizer ('=>' constantInititalizer)?
    ;
    
constant
    : literalConstant
    | magicConstant
    | classConstant
    | qualifiedNamespaceName
    | Null
    ;
    
literalConstant
    : Numeric
    | Real
    | BooleanConstant
    | stringConstant
    ;
    
classConstant
    : (Class | Parent_) '::' (identifier | Constructor | Get | Set)
    | (qualifiedStaticTypeRef | keyedVariable) '::' identifier
    ;

stringConstant
    : Label
    ;
    
string
    : StartHereDoc HereDocText+
    | StartNowDoc HereDocText+
    | DoubleQuoteString
    | SingleQuoteString
    ;

chainList
    : chain (',' chain)*
    ;

chain
    : (chainBase | functionCall | '(' newExpr ')') memberAccess*
    ;

memberAccess
    : '->' keyedFieldName actualArguments?
    ;

functionCall
    : functionCallName actualArguments
    ;

functionCallName
    : qualifiedNamespaceName
    | classConstant
    | chainBase
    ;
    
actualArguments
    : genericDynamicArgs? arguments squareCurlyExpression*
    ;

chainBase
    : keyedVariable ('::' keyedVariable)?
    | qualifiedStaticTypeRef '::' keyedVariable
    ;

keyedFieldName
    : keyedSimpleFieldName
    | keyedVariable
    ;

keyedSimpleFieldName
    : (identifier | '{' expression '}') squareCurlyExpression*
    ;

keyedVariable
    : '$'* (VarName | '$' '{' expression '}') squareCurlyExpression*
    ;
    
squareCurlyExpression
    : '[' expression? ']'
    | '{' expression '}'
    ;

assignmentList
    : assignmentListElement? (',' assignmentListElement?)*
    ;

assignmentListElement
    : chain
    | List '('  assignmentList ')'
    ;

modifier
    : Abstract
    | Final
    ;
    
identifier
    : magicConstant
    | magicMethod
    | castOperation
    | memberModifier
    | Label
    | Class
    | Interface
    | Namespace
    | Case
    | Default
    | Return
    | Import
    | Parent_
    | List
    | Function
    | Null
    | Partial
    | Echo
    | New
    | Empty
    | Callable
    | Var
    | IsSet
    | Break
    | Continue
    | Use
    | LogicalAnd
    | LogicalXor
    | LogicalOr
    | Clone
    | Global
    | Eval
    ;
    
memberModifier
    : Public
    | Protected
    | Private
    | Static
    | Abstract
    | Final
    ;
    
magicConstant
    : Namespace__
    | Class__
    | Traic__
    | Function__
    | Method__
    | Line__
    | File__
    | Dir__
    ;

magicMethod
    : Get
    | Set
    | Call
    | CallStatic
    | Constructor
    | Destruct
    | Wakeup
    | Sleep
    | Autoload
    | IsSet__
    | Unset__
    | ToString__
    | Invoke
    | SetState
    | Clone__
    | DebugInfo
    ;

primitiveType
    : BoolType
    | IntType
    | Int64Type
    | DoubleType
    | StringType
    | Resource
    | ObjectType
    | Array
    ;
    
castOperation
    : BoolType
    | Int8Cast
    | Int16Cast
    | IntType
    | Int64Type
    | Uint8Cast
    | Uint16Cast
    | Uint32Cast
    | Uint64Cast
    | DoubleCast
    | DoubleType
    | FloatCast
    | StringType
    | BinaryCast
    | UnicodeCast
    | Array
    | ObjectType
    | Resource
    | Unset
    ;
