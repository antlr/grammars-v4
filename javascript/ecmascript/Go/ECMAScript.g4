/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2014 by Bart Kiers (original author) and Adrian Herrera
 * (contributor -> ported to Python)
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
grammar ECMAScript;

@header {
import "strings"
}

@parser::members {
// here returns `true` iff on the current index of the parser's
// token stream a token of the given `type` exists on the
// `HIDDEN` channel.
//
// Args:
//  type (int): the type of the token on the `HIDDEN` channel
//      to check.
//
//  Returns:
//      `True` iff on the current index of the parser's
//      token stream a token of the given `type` exists on the
//      `HIDDEN` channel.
func (p *ECMAScriptParser) here(tokenType int) bool {
    // Get the token ahead of the current index.
    possibleIndexEosToken := p.GetCurrentToken().GetTokenIndex() - 1
    if possibleIndexEosToken < 0 {
        return false
    }
    ahead := p.GetTokenStream().Get(possibleIndexEosToken)

    // Check if the token resides on the HIDDEN channel and if it is of the
    // provided tokenType.
    return (ahead.GetChannel() == antlr.LexerHidden) && (ahead.GetTokenType() == tokenType)
}

// lineTerminatorAhead returns `true` iff on the current index of the parser's
// token stream a token exists on the `HIDDEN` channel which
// either is a line terminator, or is a multi line comment that
// contains a line terminator.
func (p *ECMAScriptParser) lineTerminatorAhead() bool {
    // Get the token ahead of the current index.
    possibleIndexEosToken := p.GetCurrentToken().GetTokenIndex() - 1
    if possibleIndexEosToken < 0 {
        return false
    }
    ahead := p.GetTokenStream().Get(possibleIndexEosToken)

    if ahead.GetChannel() != antlr.LexerHidden {
        // We're only interested in tokens on the HIDDEN channel.
        return false
    }

    if ahead.GetTokenType() == ECMAScriptParserLineTerminator {
        // There is definitely a line terminator ahead.
        return true
    }

    if ahead.GetTokenType() == ECMAScriptParserWhiteSpaces {
        // Get the token ahead of the current whitespaces.
        possibleIndexEosToken = p.GetCurrentToken().GetTokenIndex() - 2
        if possibleIndexEosToken < 0 {
            return false
        }
        ahead = p.GetTokenStream().Get(possibleIndexEosToken)
    }

    // Get the token's text and type.
    text := ahead.GetText()
    tokenType := ahead.GetTokenType()

    // Check if the token is, or contains a line terminator.
    if tokenType == ECMAScriptParserMultiLineComment && strings.ContainsAny(text, "\r\n") {
        return true
    }

    return tokenType == ECMAScriptParserLineTerminator
}
}

@lexer::members {
// unused is only here to prevent a compiler error complainaing
// that "strings" is imported but not used
func unused() {
    _ = strings.Contains("", "")
}

// A flag indicating if the lexer should operate in strict mode
// When set to true, FutureReservedWords are tokenized, when false,
// an octal literal can be tokenized
var strictMode bool = true

// The most recently produced token.
var lastToken antlr.Token

// nextToken returns the next token from the character stream and records this last
// token in case it resides on the default channel. This recorded token
// is used to determine when the lexer could possible match a regex
// literal.
func (l *ECMAScriptLexer) nextToken() antlr.Token {
    next := l.NextToken()

    if next.GetChannel() == antlr.TokenDefaultChannel {
        // Keep track of the last token on the default channel.
        lastToken = next
    }

    return next
}

// isRegexPossible returns `true` iff the lexer can match a regex literal.
func (l *ECMAScriptLexer) isRegexPossible() bool {
    if lastToken == nil {
        // No token has been produced yet: at the start of the input,
        // no division is possible, so a regex literal _is_ possible.
        return true
    }

    tokenType := lastToken.GetTokenType()
    if tokenType == ECMAScriptLexerIdentifier ||
            tokenType == ECMAScriptLexerNullLiteral ||
            tokenType == ECMAScriptLexerBooleanLiteral ||
            tokenType == ECMAScriptLexerThis ||
            tokenType == ECMAScriptLexerCloseBracket ||
            tokenType == ECMAScriptLexerCloseParen ||
            tokenType == ECMAScriptLexerOctalIntegerLiteral ||
            tokenType == ECMAScriptLexerDecimalLiteral ||
            tokenType == ECMAScriptLexerHexIntegerLiteral ||
            tokenType == ECMAScriptLexerStringLiteral ||
            tokenType == ECMAScriptLexerPlusPlus ||
            tokenType == ECMAScriptLexerMinusMinus {
        // After any of the tokens above, no regex literal can follow.
        return false
    }

    // In all other cases, a regex literal _is_ possible.
    return true
}
}

/// Program :
///     SourceElements?
program
 : sourceElements? EOF
 ;

/// SourceElements :
///     SourceElement
///     SourceElements SourceElement
sourceElements
 : sourceElement+
 ;

/// SourceElement :
///     Statement
///     FunctionDeclaration
sourceElement
 : {p.GetInputStream().LA(1) != ECMAScriptParserFunction}? statement
 | functionDeclaration
 ;

/// Statement :
///     Block
///     VariableStatement
///     EmptyStatement
///     ExpressionStatement
///     IfStatement
///     IterationStatement
///     ContinueStatement
///     BreakStatement
///     ReturnStatement
///     WithStatement
///     LabelledStatement
///     SwitchStatement
///     ThrowStatement
///     TryStatement
///     DebuggerStatement
statement
 : block
 | variableStatement
 | voidStatement
 | {p.GetInputStream().LA(1) != ECMAScriptParserOpenBrace}? expressionStatement
 | ifStatement
 | iterationStatement
 | continueStatement
 | breakStatement
 | returnStatement
 | withStatement
 | labelledStatement
 | switchStatement
 | throwStatement
 | tryStatement
 | debuggerStatement
 ;

/// Block :
///     { StatementList? }
block
 : '{' statementList? '}'
 ;

/// StatementList :
///     Statement
///     StatementList Statement
statementList
 : statement+
 ;

/// VariableStatement :
///     var VariableDeclarationList ;
variableStatement
 : Var variableDeclarationList eos
 ;

/// VariableDeclarationList :
///     VariableDeclaration
///     VariableDeclarationList , VariableDeclaration
variableDeclarationList
 : variableDeclaration ( ',' variableDeclaration )*
 ;

/// VariableDeclaration :
///     Identifier Initialiser?
variableDeclaration
 : Identifier initialiser?
 ;

/// Initialiser :
///     = AssignmentExpression
initialiser
 : '=' singleExpression
 ;

/// VoidStatement :
///     ;
voidStatement
 : SemiColon
 ;

/// ExpressionStatement :
///     [lookahead ∉ {{, function}] Expression ;
expressionStatement
 : expressionSequence eos
 ;

/// IfStatement :
///     if ( Expression ) Statement else Statement
///     if ( Expression ) Statement
ifStatement
 : If '(' expressionSequence ')' statement ( Else statement )?
 ;

/// IterationStatement :
///     do Statement while ( Expression );
///     while ( Expression ) Statement
///     for ( Expression? ; Expression? ; Expression? ) Statement
///     for ( var VariableDeclarationList ; Expression? ; Expression? ) Statement
///     for ( LeftHandSideExpression in Expression ) Statement
///     for ( var VariableDeclaration in Expression ) Statement
iterationStatement
 : Do statement While '(' expressionSequence ')' eos                                                 # DoStatement
 | While '(' expressionSequence ')' statement                                                        # WhileStatement
 | For '(' expressionSequence? ';' expressionSequence? ';' expressionSequence? ')' statement         # ForStatement
 | For '(' Var variableDeclarationList ';' expressionSequence? ';' expressionSequence? ')' statement # ForVarStatement
 | For '(' singleExpression In expressionSequence ')' statement                                      # ForInStatement
 | For '(' Var variableDeclaration In expressionSequence ')' statement                               # ForVarInStatement
 ;

/// ContinueStatement :
///     continue ;
///     continue [no LineTerminator here] Identifier ;
continueStatement
 : Continue Identifier? eos
 ;

/// BreakStatement :
///     break ;
///     break [no LineTerminator here] Identifier ;
breakStatement
 : Break Identifier? eos
 ;

/// ReturnStatement :
///     return ;
///     return [no LineTerminator here] Expression ;
returnStatement
 : Return expressionSequence? eos
 ;

/// WithStatement :
///     with ( Expression ) Statement
withStatement
 : With '(' expressionSequence ')' statement
 ;

/// SwitchStatement :
///     switch ( Expression ) CaseBlock
switchStatement
 : Switch '(' expressionSequence ')' caseBlock
 ;

/// CaseBlock :
///     { CaseClauses? }
///     { CaseClauses? DefaultClause CaseClauses? }
caseBlock
 : '{' caseClauses? ( defaultClause caseClauses? )? '}'
 ;

/// CaseClauses :
///     CaseClause
///     CaseClauses CaseClause
caseClauses
 : caseClause+
 ;

/// CaseClause :
///     case Expression ':' StatementList?
caseClause
 : Case expressionSequence ':' statementList?
 ;

/// DefaultClause :
///     default ':' StatementList?
defaultClause
 : Default ':' statementList?
 ;

/// LabelledStatement :
///     Identifier ':' Statement
labelledStatement
 : Identifier ':' statement
 ;

/// ThrowStatement :
///     throw [no LineTerminator here] Expression ;
throwStatement
 : Throw expressionSequence eos
 ;

/// TryStatement :
///     try Block Catch
///     try Block Finally
///     try Block Catch Finally
tryStatement
 : Try block catchProduction
 | Try block finallyProduction
 | Try block catchProduction finallyProduction
 ;

/// Catch :
///     catch ( Identifier ) Block
catchProduction
 : Catch '(' Identifier ')' block
 ;

/// Finally :
///     finally Block
finallyProduction
 : Finally block
 ;

/// DebuggerStatement :
///     debugger ;
debuggerStatement
 : Debugger eos
 ;

/// FunctionDeclaration :
///     function Identifier ( FormalParameterList? ) { FunctionBody }
functionDeclaration
 : Function Identifier '(' formalParameterList? ')' '{' functionBody '}'
 ;

/// FormalParameterList :
///     Identifier
///     FormalParameterList , Identifier
formalParameterList
 : Identifier ( ',' Identifier )*
 ;

/// FunctionBody :
///     SourceElements?
functionBody
 : sourceElements?
 ;

/// ArrayLiteral :
///     [ Elision? ]
///     [ ElementList ]
///     [ ElementList , Elision? ]
arrayLiteral
 : '[' elementList? ','? elision? ']'
 ;

/// ElementList :
///     Elision? AssignmentExpression
///     ElementList , Elision? AssignmentExpression
elementList
 : elision? singleExpression ( ',' elision? singleExpression )*
 ;

/// Elision :
///     ,
///     Elision ,
elision
 : ','+
 ;

/// ObjectLiteral :
///     { }
///     { PropertyNameAndValueList }
///     { PropertyNameAndValueList , }
objectLiteral
 : '{' '}'
 | '{' propertyNameAndValueList ','? '}'
 ;

/// PropertyNameAndValueList :
///     PropertyAssignment
///     PropertyNameAndValueList , PropertyAssignment
propertyNameAndValueList
 : propertyAssignment ( ',' propertyAssignment )*
 ;

/// PropertyAssignment :
///     PropertyName : AssignmentExpression
///     get PropertyName ( ) { FunctionBody }
///     set PropertyName ( PropertySetParameterList ) { FunctionBody }
propertyAssignment
 : propertyName ':' singleExpression                            # PropertyExpressionAssignment
 | getter '(' ')' '{' functionBody '}'                          # PropertyGetter
 | setter '(' propertySetParameterList ')' '{' functionBody '}' # PropertySetter
 ;

/// PropertyName :
///     IdentifierName
///     StringLiteral
///     NumericLiteral
propertyName
 : identifierName
 | StringLiteral
 | numericLiteral
 ;

/// PropertySetParameterList :
///     Identifier
propertySetParameterList
 : Identifier
 ;

/// Arguments :
///     ( )
///     ( ArgumentList )
arguments
 : '(' argumentList? ')'
 ;

/// ArgumentList :
///     AssignmentExpression
///     ArgumentList , AssignmentExpression
argumentList
 : singleExpression ( ',' singleExpression )*
 ;

/// Expression :
///     AssignmentExpression
///     Expression , AssignmentExpression
///
/// AssignmentExpression :
///     ConditionalExpression
///     LeftHandSideExpression = AssignmentExpression
///     LeftHandSideExpression AssignmentOperator AssignmentExpression
///
/// ConditionalExpression :
///     LogicalORExpression
///     LogicalORExpression ? AssignmentExpression : AssignmentExpression
///
/// LogicalORExpression :
///     LogicalANDExpression
///     LogicalORExpression || LogicalANDExpression
///
/// LogicalANDExpression :
///     BitwiseORExpression
///     LogicalANDExpression && BitwiseORExpression
///
/// BitwiseORExpression :
///     BitwiseXORExpression
///     BitwiseORExpression | BitwiseXORExpression
///
/// BitwiseXORExpression :
///     BitwiseANDExpression
///     BitwiseXORExpression ^ BitwiseANDExpression
///
/// BitwiseANDExpression :
///     EqualityExpression
///     BitwiseANDExpression & EqualityExpression
///
/// EqualityExpression :
///     RelationalExpression
///     EqualityExpression == RelationalExpression
///     EqualityExpression != RelationalExpression
///     EqualityExpression === RelationalExpression
///     EqualityExpression !== RelationalExpression
///
/// RelationalExpression :
///     ShiftExpression
///     RelationalExpression < ShiftExpression
///     RelationalExpression > ShiftExpression
///     RelationalExpression <= ShiftExpression
///     RelationalExpression >= ShiftExpression
///     RelationalExpression instanceof ShiftExpression
///     RelationalExpression in ShiftExpression
///
/// ShiftExpression :
///     AdditiveExpression
///     ShiftExpression << AdditiveExpression
///     ShiftExpression >> AdditiveExpression
///     ShiftExpression >>> AdditiveExpression
///
/// AdditiveExpression :
///     MultiplicativeExpression
///     AdditiveExpression + MultiplicativeExpression
///     AdditiveExpression - MultiplicativeExpression
///
/// MultiplicativeExpression :
///     UnaryExpression
///     MultiplicativeExpression * UnaryExpression
///     MultiplicativeExpression / UnaryExpression
///     MultiplicativeExpression % UnaryExpression
///
/// UnaryExpression :
///     PostfixExpression
///     delete UnaryExpression
///     void UnaryExpression
///     typeof UnaryExpression
///     ++ UnaryExpression
///     -- UnaryExpression
///     + UnaryExpression
///     - UnaryExpression
///     ~ UnaryExpression
///     ! UnaryExpression
///
/// PostfixExpression :
///     LeftHandSideExpression
///     LeftHandSideExpression [no LineTerminator here] ++
///     LeftHandSideExpression [no LineTerminator here] --
///
/// LeftHandSideExpression :
///     NewExpression
///     CallExpression
///
/// CallExpression :
///     MemberExpression Arguments
///     CallExpression Arguments
///     CallExpression [ Expression ]
///     CallExpression . IdentifierName
///
/// NewExpression :
///     MemberExpression
///     new NewExpression
///
/// MemberExpression :
///     PrimaryExpression
///     FunctionExpression
///     MemberExpression [ Expression ]
///     MemberExpression . IdentifierName
///     new MemberExpression Arguments
///
/// FunctionExpression :
///     function Identifier? ( FormalParameterList? ) { FunctionBody }
///
/// PrimaryExpression :
///     this
///     Identifier
///     Literal
///     ArrayLiteral
///     ObjectLiteral
///     ( Expression )
///
expressionSequence
 : singleExpression ( ',' singleExpression )*
 ;

singleExpression
 : Function Identifier? '(' formalParameterList? ')' '{' functionBody '}' # FunctionExpression
 | singleExpression '[' expressionSequence ']'                            # MemberIndexExpression
 | singleExpression '.' identifierName                                    # MemberDotExpression
 | singleExpression arguments                                             # ArgumentsExpression
 | New singleExpression arguments?                                        # NewExpression
 | singleExpression {!p.here(ECMAScriptParserLineTerminator)}? '++'       # PostIncrementExpression
 | singleExpression {!p.here(ECMAScriptParserLineTerminator)}? '--'       # PostDecreaseExpression
 | Delete singleExpression                                                # DeleteExpression
 | Void singleExpression                                                  # VoidExpression
 | Typeof singleExpression                                                # TypeofExpression
 | '++' singleExpression                                                  # PreIncrementExpression
 | '--' singleExpression                                                  # PreDecreaseExpression
 | '+' singleExpression                                                   # UnaryPlusExpression
 | '-' singleExpression                                                   # UnaryMinusExpression
 | '~' singleExpression                                                   # BitNotExpression
 | '!' singleExpression                                                   # NotExpression
 | singleExpression ( '*' | '/' | '%' ) singleExpression                  # MultiplicativeExpression
 | singleExpression ( '+' | '-' ) singleExpression                        # AdditiveExpression
 | singleExpression ( '<<' | '>>' | '>>>' ) singleExpression              # BitShiftExpression
 | singleExpression ( '<' | '>' | '<=' | '>=' ) singleExpression          # RelationalExpression
 | singleExpression Instanceof singleExpression                           # InstanceofExpression
 | singleExpression In singleExpression                                   # InExpression
 | singleExpression ( '==' | '!=' | '===' | '!==' ) singleExpression      # EqualityExpression
 | singleExpression '&' singleExpression                                  # BitAndExpression
 | singleExpression '^' singleExpression                                  # BitXOrExpression
 | singleExpression '|' singleExpression                                  # BitOrExpression
 | singleExpression '&&' singleExpression                                 # LogicalAndExpression
 | singleExpression '||' singleExpression                                 # LogicalOrExpression
 | singleExpression '?' singleExpression ':' singleExpression             # TernaryExpression
 | singleExpression '=' singleExpression                                  # AssignmentExpression
 | singleExpression assignmentOperator singleExpression                   # AssignmentOperatorExpression
 | This                                                                   # ThisExpression
 | Identifier                                                             # IdentifierExpression
 | literal                                                                # LiteralExpression
 | arrayLiteral                                                           # ArrayLiteralExpression
 | objectLiteral                                                          # ObjectLiteralExpression
 | '(' expressionSequence ')'                                             # ParenthesizedExpression
 ;

/// AssignmentOperator : one of
///     *=	/=	%=	+=	-=	<<=	>>=	>>>=	&=	^=	|=
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
 ;

literal
 : ( NullLiteral
   | BooleanLiteral
   | StringLiteral
   | RegularExpressionLiteral
   )
 | numericLiteral
 ;

numericLiteral
 : DecimalLiteral
 | HexIntegerLiteral
 | OctalIntegerLiteral
 ;

identifierName
 : Identifier
 | reservedWord
 ;

reservedWord
 : keyword
 | futureReservedWord
 | ( NullLiteral
   | BooleanLiteral
   )
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
 | Function
 | This
 | With
 | Default
 | If
 | Throw
 | Delete
 | In
 | Try
 ;

futureReservedWord
 : Class
 | Enum
 | Extends
 | Super
 | Const
 | Export
 | Import
 | Implements
 | Let
 | Private
 | Public
 | Interface
 | Package
 | Protected
 | Static
 | Yield
 ;

getter
 : {p.GetTokenStream().LT(1).GetText() == "get"}? Identifier propertyName
 ;

setter
 : {p.GetTokenStream().LT(1).GetText() == "set"}? Identifier propertyName
 ;

eos
 : SemiColon
 | EOF
 | {p.lineTerminatorAhead()}?
 | {p.GetTokenStream().LT(1).GetTokenType() == ECMAScriptParserCloseBrace}?
 ;

eof
 : EOF
 ;

/// RegularExpressionLiteral ::
///     / RegularExpressionBody / RegularExpressionFlags
RegularExpressionLiteral
 : {p.isRegexPossible()}? '/' RegularExpressionBody '/' RegularExpressionFlags
 ;

/// 7.3 Line Terminators
LineTerminator
 : [\r\n\u2028\u2029] -> channel(HIDDEN)
 ;

OpenBracket                : '[';
CloseBracket               : ']';
OpenParen                  : '(';
CloseParen                 : ')';
OpenBrace                  : '{';
CloseBrace                 : '}';
SemiColon                  : ';';
Comma                      : ',';
Assign                     : '=';
QuestionMark               : '?';
Colon                      : ':';
Dot                        : '.';
PlusPlus                   : '++';
MinusMinus                 : '--';
Plus                       : '+';
Minus                      : '-';
BitNot                     : '~';
Not                        : '!';
Multiply                   : '*';
Divide                     : '/';
Modulus                    : '%';
RightShiftArithmetic       : '>>';
LeftShiftArithmetic        : '<<';
RightShiftLogical          : '>>>';
LessThan                   : '<';
MoreThan                   : '>';
LessThanEquals             : '<=';
GreaterThanEquals          : '>=';
Equals                     : '==';
NotEquals                  : '!=';
IdentityEquals             : '===';
IdentityNotEquals          : '!==';
BitAnd                     : '&';
BitXOr                     : '^';
BitOr                      : '|';
And                        : '&&';
Or                         : '||';
MultiplyAssign             : '*=';
DivideAssign               : '/=';
ModulusAssign              : '%=';
PlusAssign                 : '+=';
MinusAssign                : '-=';
LeftShiftArithmeticAssign  : '<<=';
RightShiftArithmeticAssign : '>>=';
RightShiftLogicalAssign    : '>>>=';
BitAndAssign               : '&=';
BitXorAssign               : '^=';
BitOrAssign                : '|=';

/// 7.8.1 Null Literals
NullLiteral
 : 'null'
 ;

/// 7.8.2 Boolean Literals
BooleanLiteral
 : 'true'
 | 'false'
 ;

/// 7.8.3 Numeric Literals
DecimalLiteral
 : DecimalIntegerLiteral '.' DecimalDigit* ExponentPart?
 | '.' DecimalDigit+ ExponentPart?
 | DecimalIntegerLiteral ExponentPart?
 ;

/// 7.8.3 Numeric Literals
HexIntegerLiteral
 : '0' [xX] HexDigit+
 ;

OctalIntegerLiteral
 : {!strictMode}? '0' OctalDigit+
 ;

/// 7.6.1.1 Keywords
Break      : 'break';
Do         : 'do';
Instanceof : 'instanceof';
Typeof     : 'typeof';
Case       : 'case';
Else       : 'else';
New        : 'new';
Var        : 'var';
Catch      : 'catch';
Finally    : 'finally';
Return     : 'return';
Void       : 'void';
Continue   : 'continue';
For        : 'for';
Switch     : 'switch';
While      : 'while';
Debugger   : 'debugger';
Function   : 'function';
This       : 'this';
With       : 'with';
Default    : 'default';
If         : 'if';
Throw      : 'throw';
Delete     : 'delete';
In         : 'in';
Try        : 'try';

/// 7.6.1.2 Future Reserved Words
Class   : 'class';
Enum    : 'enum';
Extends : 'extends';
Super   : 'super';
Const   : 'const';
Export  : 'export';
Import  : 'import';

/// The following tokens are also considered to be FutureReservedWords
/// when parsing strict mode
Implements : {strictMode}? 'implements';
Let        : {strictMode}? 'let';
Private    : {strictMode}? 'private';
Public     : {strictMode}? 'public';
Interface  : {strictMode}? 'interface';
Package    : {strictMode}? 'package';
Protected  : {strictMode}? 'protected';
Static     : {strictMode}? 'static';
Yield      : {strictMode}? 'yield';

/// 7.6 Identifier Names and Identifiers
Identifier
 : IdentifierStart IdentifierPart*
 ;

/// 7.8.4 String Literals
StringLiteral
 : '"' DoubleStringCharacter* '"'
 | '\'' SingleStringCharacter* '\''
 ;

WhiteSpaces
 : [\t\u000B\u000C\u0020\u00A0]+ -> channel(HIDDEN)
 ;

/// 7.4 Comments
MultiLineComment
 : '/*' .*? '*/' -> channel(HIDDEN)
 ;

SingleLineComment
 : '//' ~[\r\n\u2028\u2029]* -> channel(HIDDEN)
 ;

UnexpectedCharacter
 : .
 ;

fragment DoubleStringCharacter
 : ~["\\\r\n]
 | '\\' EscapeSequence
 | LineContinuation
 ;

fragment SingleStringCharacter
 : ~['\\\r\n]
 | '\\' EscapeSequence
 | LineContinuation
 ;

fragment EscapeSequence
 : CharacterEscapeSequence
 | '0' // no digit ahead! TODO
 | HexEscapeSequence
 | UnicodeEscapeSequence
 ;

fragment CharacterEscapeSequence
 : SingleEscapeCharacter
 | NonEscapeCharacter
 ;

fragment HexEscapeSequence
 : 'x' HexDigit HexDigit
 ;

fragment UnicodeEscapeSequence
 : 'u' HexDigit HexDigit HexDigit HexDigit
 ;

fragment SingleEscapeCharacter
 : ['"\\bfnrtv]
 ;

fragment NonEscapeCharacter
 : ~['"\\bfnrtv0-9xu\r\n]
 ;

fragment EscapeCharacter
 : SingleEscapeCharacter
 | DecimalDigit
 | [xu]
 ;

fragment LineContinuation
 : '\\' LineTerminatorSequence
 ;

fragment LineTerminatorSequence
 : '\r\n'
 | LineTerminator
 ;

fragment DecimalDigit
 : [0-9]
 ;

fragment HexDigit
 : [0-9a-fA-F]
 ;

fragment OctalDigit
 : [0-7]
 ;

fragment DecimalIntegerLiteral
 : '0'
 | [1-9] DecimalDigit*
 ;

fragment ExponentPart
 : [eE] [+-]? DecimalDigit+
 ;

fragment IdentifierStart
 : [\p{L}]
 | [$_]
 | '\\' UnicodeEscapeSequence
 ;

fragment IdentifierPart
 : IdentifierStart
 | [\p{Mn}]
 | [\p{Nd}]
 | [\p{Pc}]
 | ZWNJ
 | ZWJ
 ;

fragment ZWNJ
 : '\u200C'
 ;

fragment ZWJ
 : '\u200D'
 ;

/// RegularExpressionBody ::
///     RegularExpressionFirstChar RegularExpressionChars
///
/// RegularExpressionChars ::
///     [empty]
///     RegularExpressionChars RegularExpressionChar
fragment RegularExpressionBody
 : RegularExpressionFirstChar RegularExpressionChar*
 ;

/// RegularExpressionFlags ::
///     [empty]
///     RegularExpressionFlags IdentifierPart
fragment RegularExpressionFlags
 : IdentifierPart*
 ;

/// RegularExpressionFirstChar ::
///     RegularExpressionNonTerminator but not one of * or \ or / or [
///     RegularExpressionBackslashSequence
///     RegularExpressionClass
fragment RegularExpressionFirstChar
 : ~[\r\n\u2028\u2029*\\/[]
 | RegularExpressionBackslashSequence
 | RegularExpressionClass
 ;

/// RegularExpressionChar ::
///     RegularExpressionNonTerminator but not \ or / or [
///     RegularExpressionBackslashSequence
///     RegularExpressionClass
fragment RegularExpressionChar
 : ~[\r\n\u2028\u2029\\/[]
 | RegularExpressionBackslashSequence
 | RegularExpressionClass
 ;

/// RegularExpressionNonTerminator ::
///     SourceCharacter but not LineTerminator
fragment RegularExpressionNonTerminator
 : ~[\r\n\u2028\u2029]
 ;

/// RegularExpressionBackslashSequence ::
///     \ RegularExpressionNonTerminator
fragment RegularExpressionBackslashSequence
 : '\\' RegularExpressionNonTerminator
 ;

/// RegularExpressionClass ::
///     [ RegularExpressionClassChars ]
///
/// RegularExpressionClassChars ::
///     [empty]
///     RegularExpressionClassChars RegularExpressionClassChar
fragment RegularExpressionClass
  : '[' RegularExpressionClassChar* ']'
  ;

/// RegularExpressionClassChar ::
///     RegularExpressionNonTerminator but not ] or \
///     RegularExpressionBackslashSequence
fragment RegularExpressionClassChar
 : ~[\r\n\u2028\u2029\]\\]
 | RegularExpressionBackslashSequence
 ;
