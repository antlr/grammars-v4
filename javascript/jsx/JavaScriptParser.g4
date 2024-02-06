/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2014 by Bart Kiers (original author) and Alexandre Vitorelli (contributor -> ported to CSharp)
 * Copyright (c) 2017-2020 by Ivan Kochurkin (Positive Technologies):
    added ECMAScript 6 support, cleared and transformed to the universal grammar.
 * Copyright (c) 2018 by Juan Alvarez (contributor -> ported to Go)
 * Copyright (c) 2019 by Student Main (contributor -> ES2020)
 * Copyright (c) 2024 by Andrew Leppard (www.wegrok.review)
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar JavaScriptParser;

options {
    tokenVocab = JavaScriptLexer;
    superClass = JavaScriptParserBase;
}

program
    : HashBangLine? sourceElements? EOF
    ;

sourceElement
    : statement
    ;

statement
    : block
    | variableStatement
    | importStatement
    | exportStatement
    | emptyStatement_
    | classDeclaration
    | expressionStatement
    | ifStatement
    | iterationStatement
    | continueStatement
    | breakStatement
    | returnStatement
    | yieldStatement
    | withStatement
    | labelledStatement
    | switchStatement
    | throwStatement
    | tryStatement
    | debuggerStatement
    | functionDeclaration
    ;

block
    : OpenBrace statementList? CloseBrace
    ;

statementList
    : statement+
    ;

importStatement
    : Import importFromBlock
    ;

importFromBlock
    : importDefault? (importNamespace | moduleItems) importFrom eos
    | StringLiteral eos
    ;

moduleItems
    : OpenBrace (aliasName ',')* (aliasName ','?)? CloseBrace
    ;

importDefault
    : aliasName ','
    ;

importNamespace
    : ('*' | identifierName) (As identifierName)?
    ;

importFrom
    : From StringLiteral
    ;

aliasName
    : identifierName (As identifierName)?
    ;

exportStatement
    : Export (exportFromBlock | declaration) eos # ExportDeclaration
    | Export Default singleExpression eos        # ExportDefaultDeclaration
    ;

exportFromBlock
    : importNamespace importFrom eos
    | moduleItems importFrom? eos
    ;

declaration
    : variableStatement
    | classDeclaration
    | functionDeclaration
    ;

variableStatement
    : variableDeclarationList eos
    ;

variableDeclarationList
    : varModifier variableDeclaration (',' variableDeclaration)*
    ;

variableDeclaration
    : assignable (Assign singleExpression)? // ECMAScript 6: Array & Object Matching
    ;

emptyStatement_
    : SemiColon
    ;

expressionStatement
    : {this.notOpenBraceAndNotFunction()}? expressionSequence eos
    ;

ifStatement
    : If '(' expressionSequence ')' statement (Else statement)?
    ;

iterationStatement
    : Do statement While '(' expressionSequence ')' eos                                                                     # DoStatement
    | While '(' expressionSequence ')' statement                                                                            # WhileStatement
    | For '(' (expressionSequence | variableDeclarationList)? ';' expressionSequence? ';' expressionSequence? ')' statement # ForStatement
    | For '(' (singleExpression | variableDeclarationList) In expressionSequence ')' statement                              # ForInStatement
    // strange, 'of' is an identifier. and this.p("of") not work in sometime.
    | For Await? '(' (singleExpression | variableDeclarationList) identifier {this.p("of")}? expressionSequence ')' statement # ForOfStatement
    ;

varModifier // let, const - ECMAScript 6
    : Var
    | let_
    | Const
    ;

continueStatement
    : Continue ({this.notLineTerminator()}? identifier)? eos
    ;

breakStatement
    : Break ({this.notLineTerminator()}? identifier)? eos
    ;

returnStatement
    : Return ({this.notLineTerminator()}? expressionSequence)? eos
    | Return '(' jsxElements ')' eos
    ;

yieldStatement
    : Yield ({this.notLineTerminator()}? expressionSequence)? eos
    ;

withStatement
    : With '(' expressionSequence ')' statement
    ;

switchStatement
    : Switch '(' expressionSequence ')' caseBlock
    ;

caseBlock
    : OpenBrace caseClauses? (defaultClause caseClauses?)? CloseBrace
    ;

caseClauses
    : caseClause+
    ;

caseClause
    : Case expressionSequence ':' statementList?
    ;

defaultClause
    : Default ':' statementList?
    ;

labelledStatement
    : identifier ':' statement
    ;

throwStatement
    : Throw {this.notLineTerminator()}? expressionSequence eos
    ;

tryStatement
    : Try block (catchProduction finallyProduction? | finallyProduction)
    ;

catchProduction
    : Catch ('(' assignable? ')')? block
    ;

finallyProduction
    : Finally block
    ;

debuggerStatement
    : Debugger eos
    ;

functionDeclaration
    : Async? Function_ '*'? identifier '(' formalParameterList? ')' OpenBrace functionBody CloseBrace
    ;

classDeclaration
    : Class identifier classTail
    ;

classTail
    : (Extends singleExpression)? OpenBrace classElement* CloseBrace
    ;

classElement
    : (Static | {this.n("static")}? identifier | Async)* (
        methodDefinition
        | assignable Assign objectLiteral ';'
    )
    | emptyStatement_
    | '#'? propertyName Assign singleExpression
    ;

methodDefinition
    : '*'? '#'? propertyName '(' formalParameterList? ')' OpenBrace functionBody CloseBrace
    | '*'? '#'? getter '(' ')' OpenBrace functionBody CloseBrace
    | '*'? '#'? setter '(' formalParameterList? ')' OpenBrace functionBody CloseBrace
    ;

formalParameterList
    : formalParameterArg (',' formalParameterArg)* (',' lastFormalParameterArg)?
    | lastFormalParameterArg
    ;

formalParameterArg
    : assignable (Assign singleExpression)? // ECMAScript 6: Initialization
    ;

lastFormalParameterArg // ECMAScript 6: Rest Parameter
    : Ellipsis singleExpression
    ;

functionBody
    : sourceElements?
    ;

sourceElements
    : sourceElement+
    ;

arrayLiteral
    : ('[' elementList ']')
    ;

elementList
    : ','* arrayElement? (','+ arrayElement)* ','* // Yes, everything is optional
    ;

arrayElement
    : Ellipsis? singleExpression
    ;

propertyAssignment
    : propertyName ':' singleExpression                                          # PropertyExpressionAssignment
    | '[' singleExpression ']' ':' singleExpression                              # ComputedPropertyExpressionAssignment
    | Async? '*'? propertyName '(' formalParameterList? ')' OpenBrace functionBody CloseBrace # FunctionProperty
    | getter '(' ')' OpenBrace functionBody CloseBrace                                        # PropertyGetter
    | setter '(' formalParameterArg ')' OpenBrace functionBody CloseBrace                     # PropertySetter
    | Ellipsis? singleExpression                                                 # PropertyShorthand
    ;

propertyName
    : identifierName
    | StringLiteral
    | numericLiteral
    | '[' singleExpression ']'
    ;

arguments
    : '(' (argument (',' argument)* ','?)? ')'
    ;

argument
    : Ellipsis? (singleExpression | identifier)
    ;

expressionSequence
    : Ellipsis? singleExpression (',' Ellipsis? singleExpression)* //2020/10/28 add SpreadExpr for htmltag
    ;

singleExpression
    : anoymousFunction                              # FunctionExpression
    | Class identifier? classTail                   # ClassExpression
    | singleExpression '[' expressionSequence ']'   # MemberIndexExpression
    | singleExpression '?'? '.' '#'? identifierName # MemberDotExpression
    // Split to try `new Date()` first, then `new Date`.
    | New singleExpression arguments                                       # NewExpression
    | New singleExpression                                                 # NewExpression
    | singleExpression arguments                                           # ArgumentsExpression
    | New '.' identifier                                                   # MetaExpression // new.target
    | singleExpression {this.notLineTerminator()}? '++'                    # PostIncrementExpression
    | singleExpression {this.notLineTerminator()}? '--'                    # PostDecreaseExpression
    | Delete singleExpression                                              # DeleteExpression
    | Void singleExpression                                                # VoidExpression
    | Typeof singleExpression                                              # TypeofExpression
    | '++' singleExpression                                                # PreIncrementExpression
    | '--' singleExpression                                                # PreDecreaseExpression
    | '+' singleExpression                                                 # UnaryPlusExpression
    | '-' singleExpression                                                 # UnaryMinusExpression
    | '~' singleExpression                                                 # BitNotExpression
    | '!' singleExpression                                                 # NotExpression
    | Await singleExpression                                               # AwaitExpression
    | <assoc = right> singleExpression '**' singleExpression               # PowerExpression
    | singleExpression ('*' | '/' | '%') singleExpression                  # MultiplicativeExpression
    | singleExpression ('+' | '-') singleExpression                        # AdditiveExpression
    | singleExpression '??' singleExpression                               # CoalesceExpression
    | singleExpression ('<<' | '>>' | '>>>') singleExpression              # BitShiftExpression
    | singleExpression (LessThan | MoreThan | '<=' | '>=') singleExpression # RelationalExpression
    | singleExpression Instanceof singleExpression                         # InstanceofExpression
    | singleExpression In singleExpression                                 # InExpression
    | singleExpression ('==' | '!=' | '===' | '!==') singleExpression      # EqualityExpression
    | singleExpression '&' singleExpression                                # BitAndExpression
    | singleExpression '^' singleExpression                                # BitXOrExpression
    | singleExpression '|' singleExpression                                # BitOrExpression
    | singleExpression '&&' singleExpression                               # LogicalAndExpression
    | singleExpression '||' singleExpression                               # LogicalOrExpression
    | singleExpression '?' singleExpression ':' singleExpression           # TernaryExpression
    | <assoc = right> singleExpression Assign singleExpression             # AssignmentExpression
    | <assoc = right> singleExpression assignmentOperator singleExpression # AssignmentOperatorExpression
    | Import '(' singleExpression ')'                                      # ImportExpression
    | singleExpression templateStringLiteral                               # TemplateStringExpression // ECMAScript 6
    | yieldStatement                                                       # YieldExpression          // ECMAScript 6
    | This                                                                 # ThisExpression
    | identifier                                                           # IdentifierExpression
    | Super                                                                # SuperExpression
    | literal                                                              # LiteralExpression
    | arrayLiteral                                                         # ArrayLiteralExpression
    | objectLiteral                                                        # ObjectLiteralExpression
    | jsxElements                                                          # jsxElementExpression
    | '(' expressionSequence ')'                                           # ParenthesizedExpression
    ;

jsxElements
    : jsxElement+
    ;

jsxElementBegin
    : JsxElementBegin
    | JsxOpeningElementBegin
    | JsxChildrenOpeningElementBegin
    ;

jsxElement
    : jsxSelfClosingElement
    | jsxOpeningElement jsxChildren jsxClosingElement
    ;

jsxSelfClosingElement
    : jsxElementBegin jsxSelfClosingElementName jsxAttributes? JsxOpeningElementSlashEnd
    ;

jsxOpeningElement
    : jsxElementBegin jsxOpeningElementName jsxAttributes? JsxOpeningElementEnd
    ;

jsxClosingElement
    : JsxChildrenClosingElementSlashBegin jsxClosingElementName JsxClosingElementEnd
    ;

jsxChildren
    : HtmlChardata? ((jsxElement | objectExpressionSequence) HtmlChardata?)*
    ;

jsxSelfClosingElementName
    : JsxOpeningElementId
    ;

jsxOpeningElementName
    : JsxOpeningElementId {this.pushHtmlTagName($JsxOpeningElementId.text);}
    ;

jsxClosingElementName
    : JsxClosingElementId {this.popHtmlTagName($JsxClosingElementId.text)}?
    ;

jsxAttributes
    : jsxSpreadAttribute jsxAttributes?
    | jsxAttribute jsxAttributes?
    ;

jsxSpreadAttribute
    : JsxOpeningElementOpenBrace Ellipsis singleExpression CloseBrace
    ;

jsxAttribute
    : jsxAttributeName JsxAssign jsxAttributeValue
    | jsxAttributeName
    ;

jsxAttributeName
    : JsxOpeningElementId
    ;

jsxAttributeValue
    : JsxAttributeValue
    | jsxElement
    | objectExpressionSequence
    ;

assignable
    : identifier
    | arrayLiteral
    | objectLiteral
    ;

objectLiteral
    : OpenBrace (propertyAssignment (',' propertyAssignment)* ','?)? CloseBrace
    ;

openBrace
    : OpenBrace
    | JsxOpeningElementOpenBrace    
    | JsxChildrenOpenBrace
    ;

objectExpressionSequence
    : openBrace expressionSequence CloseBrace
    ;

anoymousFunction
    : functionDeclaration                                                     # FunctionDecl
    | Async? Function_ '*'? '(' formalParameterList? ')' OpenBrace functionBody CloseBrace # AnoymousFunctionDecl
    | Async? arrowFunctionParameters '=>' arrowFunctionBody                   # ArrowFunction
    ;

arrowFunctionParameters
    : identifier
    | '(' formalParameterList? ')'
    ;

arrowFunctionBody
    : singleExpression
    | OpenBrace functionBody CloseBrace
    ;

assignmentOperator
    : '*='
    | '/='
    | '%='
    | '+='
    | '-='
    | '<<='
    | '>>='
    | '>>>='
    | '&='
    | '^='
    | '|='
    | '**='
    ;

literal
    : NullLiteral
    | BooleanLiteral
    | StringLiteral
    | templateStringLiteral
    | RegularExpressionLiteral
    | numericLiteral
    | bigintLiteral
    ;

templateStringLiteral
    : BackTick templateStringAtom* BackTick
    ;

templateStringAtom
    : TemplateStringAtom
    | TemplateStringStartExpression singleExpression CloseBrace
    ;

numericLiteral
    : DecimalLiteral
    | HexIntegerLiteral
    | OctalIntegerLiteral
    | OctalIntegerLiteral2
    | BinaryIntegerLiteral
    ;

bigintLiteral
    : BigDecimalIntegerLiteral
    | BigHexIntegerLiteral
    | BigOctalIntegerLiteral
    | BigBinaryIntegerLiteral
    ;

getter
    : identifier {this.p("get")}? propertyName
    ;

setter
    : identifier {this.p("set")}? propertyName
    ;

identifierName
    : identifier
    | reservedWord
    ;

identifier
    : Identifier
    | NonStrictLet
    | Async
    ;

reservedWord
    : keyword
    | NullLiteral
    | BooleanLiteral
    ;

keyword
    : Break
    | Do
    | Instanceof
    | Typeof
    | Case
    | Else
    | New
    | Var
    | Catch
    | Finally
    | Return
    | Void
    | Continue
    | For
    | Switch
    | While
    | Debugger
    | Function_
    | This
    | With
    | Default
    | If
    | Throw
    | Delete
    | In
    | Try
    | Class
    | Enum
    | Extends
    | Super
    | Const
    | Export
    | Import
    | Implements
    | let_
    | Private
    | Public
    | Interface
    | Package
    | Protected
    | Static
    | Yield
    | Async
    | Await
    | From
    | As
    ;

let_
    : NonStrictLet
    | StrictLet
    ;

eos
    : SemiColon
    | EOF
    | {this.lineTerminatorAhead()}?
    | {this.closeBrace()}?
    ;