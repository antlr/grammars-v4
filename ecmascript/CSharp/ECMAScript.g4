/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2014 by Bart Kiers (original author) and Alexandre Vitorelli (contributor -> ported to CSharp)
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

@parser::members {
  
    ///<summary>Returns <c>true</c> iff on the current index of the parser's
    ///token stream a token of the given <c>type</c> exists on the
    ///<c>Hidden</c> channel.</summary>
    ///<param name="type">the type of the token on the <c>Hidden</c> channel
    ///to check.</param>
    ///<returns><c>true</c> iff on the current index of the parser's
    ///token stream a token of the given <c>type</c> exists on the
    ///<c>Hidden</c> channel.</returns>
    private bool here(int type) {

        // Get the token ahead of the current index.
        int possibleIndexEosToken = this.CurrentToken.TokenIndex - 1;
        IToken ahead = _input.Get(possibleIndexEosToken);

        // Check if the token resides on the Hidden channel and if it's of the
        // provided type.
        return (ahead.Channel == Lexer.Hidden) && (ahead.Type == type);
    }

    ///<summary>Returns <c>true</c> iff on the current index of the parser's
    ///token stream a token exists on the <c>Hidden</c> channel which
    ///either is a line terminator, or is a multi line comment that
    ///contains a line terminator.</summary>
    ///<returns><c>true</c> iff on the current index of the parser's
    ///token stream a token exists on the <c>Hidden</c> channel which
    ///either is a line terminator, or is a multi line comment that
    ///contains a line terminator.</returns>
    private bool lineTerminatorAhead() {

        // Get the token ahead of the current index.
        int possibleIndexEosToken = this.CurrentToken.TokenIndex - 1;
        IToken ahead = _input.Get(possibleIndexEosToken);

        if (ahead.Channel != Lexer.Hidden) {
            // We're only interested in tokens on the Hidden channel.
            return false;
        }

        if (ahead.Type == LineTerminator) {
            // There is definitely a line terminator ahead.
            return true;
        }

        if (ahead.Type == WhiteSpaces) {
            // Get the token ahead of the current whitespaces.
            possibleIndexEosToken = this.CurrentToken.TokenIndex - 2;
            ahead = _input.Get(possibleIndexEosToken);
        }

        // Get the token's text and type.
        string text = ahead.Text;
        int type = ahead.Type;

        // Check if the token is, or contains a line terminator.
        return (type == MultiLineComment && (text.Contains("\r") || text.Contains("\n"))) ||
                (type == LineTerminator);
    }                                
}

@lexer::members {
                 
    // A flag indicating if the lexer should operate in strict mode.
    // When set to true, FutureReservedWords are tokenized, when false,
    // an octal literal can be tokenized.
    private bool strictMode = true;

    // The most recently produced token.
    private IToken lastToken = null;

    ///<summary>Returns <c>true</c> iff the lexer operates in strict mode</summary>
    /// <returns><c>true</c> iff the lexer operates in strict mode.</returns>
    public bool GetStrictMode() {
        return this.strictMode;
    }

	///<summary>Sets whether the lexer operates in strict mode or not.</summary>
	///<param name="strictMode">the flag indicating the lexer operates in strict mode or not.</param>
    public void SetStrictMode(bool strictMode) {
        this.strictMode = strictMode;
    }

    ///<summary>Return the next token from the character stream and records this last
    ///token in case it resides on the default channel. This recorded token
    ///is used to determine when the lexer could possibly match a regex
    ///literal.</summary>
    ///<returns>the next token from the character stream.</returns>
    public override IToken NextToken() {
        
        // Get the next token.
        IToken next = base.NextToken();
        
        if (next.Channel == Lexer.DefaultTokenChannel) {
            // Keep track of the last token on the default channel.                                              
            this.lastToken = next;
        }
        
        return next;
    }

    ///<summary>Returns <c>true</c> iff the lexer can match a regex literal.</summary>
    ///<returns><c>true</c> iff the lexer can match a regex literal.</returns>
    private bool isRegexPossible() {
                                       
        if (this.lastToken == null) {
            // No token has been produced yet: at the start of the input,
            // no division is possible, so a regex literal _is_ possible.
            return true;
        }
        
        switch (this.lastToken.Type) {
            case Identifier:
            case NullLiteral:
            case BooleanLiteral:
            case This:
            case CloseBracket:
            case CloseParen:
            case OctalIntegerLiteral:
            case DecimalLiteral:
            case HexIntegerLiteral:
            case StringLiteral:
            case PlusPlus:
            case MinusMinus:
                // After any of the tokens above, no regex literal can follow.
                return false;
            default:
                // In all other cases, a regex literal _is_ possible.
                return true;
        }
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
 : statement
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
 | emptyStatement
 | expressionStatement
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

/// EmptyStatement :
///     ;
emptyStatement
 : SemiColon
 ;

/// ExpressionStatement :
///     [lookahead ∉ {{, function}] Expression ;
expressionStatement
 : {(_input.La(1) != OpenBrace) && (_input.La(1) != Function)}? expressionSequence eos
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
 : Continue ({!here(LineTerminator)}? Identifier)? eos
 ;

/// BreakStatement :
///     break ;
///     break [no LineTerminator here] Identifier ;
breakStatement
 : Break ({!here(LineTerminator)}? Identifier)? eos
 ;

/// ReturnStatement :
///     return ;
///     return [no LineTerminator here] Expression ;
returnStatement
 : Return ({!here(LineTerminator)}? expressionSequence)? eos
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
 : Throw {!here(LineTerminator)}? expressionSequence eos
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
 | singleExpression {!here(LineTerminator)}? '++'                         # PostIncrementExpression
 | singleExpression {!here(LineTerminator)}? '--'                         # PostDecreaseExpression
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
 : {_input.Lt(1).Text.Equals("get")}? Identifier propertyName
 ;

setter
 : {_input.Lt(1).Text.Equals("set")}? Identifier propertyName
 ;

eos
 : SemiColon
 | EOF
 | {lineTerminatorAhead()}?
 | {_input.Lt(1).Type == CloseBrace}?
 ;

eof
 : EOF
 ;

/// RegularExpressionLiteral ::
///     / RegularExpressionBody / RegularExpressionFlags
RegularExpressionLiteral
 : {isRegexPossible()}? '/' RegularExpressionBody '/' RegularExpressionFlags
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
 : UnicodeLetter
 | [$_]
 | '\\' UnicodeEscapeSequence
 ;

fragment IdentifierPart
 : IdentifierStart
 | UnicodeCombiningMark
 | UnicodeDigit
 | UnicodeConnectorPunctuation
 | ZWNJ
 | ZWJ
 ;

fragment UnicodeLetter
 : [\u0041-\u005A]
 | [\u0061-\u007A]
 | [\u00AA]
 | [\u00B5]
 | [\u00BA]
 | [\u00C0-\u00D6]
 | [\u00D8-\u00F6]
 | [\u00F8-\u021F]
 | [\u0222-\u0233]
 | [\u0250-\u02AD]
 | [\u02B0-\u02B8]
 | [\u02BB-\u02C1]
 | [\u02D0-\u02D1]
 | [\u02E0-\u02E4]
 | [\u02EE]
 | [\u037A]
 | [\u0386]
 | [\u0388-\u038A]
 | [\u038C]
 | [\u038E-\u03A1]
 | [\u03A3-\u03CE]
 | [\u03D0-\u03D7]
 | [\u03DA-\u03F3]
 | [\u0400-\u0481]
 | [\u048C-\u04C4]
 | [\u04C7-\u04C8]
 | [\u04CB-\u04CC]
 | [\u04D0-\u04F5]
 | [\u04F8-\u04F9]
 | [\u0531-\u0556]
 | [\u0559]
 | [\u0561-\u0587]
 | [\u05D0-\u05EA]
 | [\u05F0-\u05F2]
 | [\u0621-\u063A]
 | [\u0640-\u064A]
 | [\u0671-\u06D3]
 | [\u06D5]
 | [\u06E5-\u06E6]
 | [\u06FA-\u06FC]
 | [\u0710]
 | [\u0712-\u072C]
 | [\u0780-\u07A5]
 | [\u0905-\u0939]
 | [\u093D]
 | [\u0950]
 | [\u0958-\u0961]
 | [\u0985-\u098C]
 | [\u098F-\u0990]
 | [\u0993-\u09A8]
 | [\u09AA-\u09B0]
 | [\u09B2]
 | [\u09B6-\u09B9]
 | [\u09DC-\u09DD]
 | [\u09DF-\u09E1]
 | [\u09F0-\u09F1]
 | [\u0A05-\u0A0A]
 | [\u0A0F-\u0A10]
 | [\u0A13-\u0A28]
 | [\u0A2A-\u0A30]
 | [\u0A32-\u0A33]
 | [\u0A35-\u0A36]
 | [\u0A38-\u0A39]
 | [\u0A59-\u0A5C]
 | [\u0A5E]
 | [\u0A72-\u0A74]
 | [\u0A85-\u0A8B]
 | [\u0A8D]
 | [\u0A8F-\u0A91]
 | [\u0A93-\u0AA8]
 | [\u0AAA-\u0AB0]
 | [\u0AB2-\u0AB3]
 | [\u0AB5-\u0AB9]
 | [\u0ABD]
 | [\u0AD0]
 | [\u0AE0]
 | [\u0B05-\u0B0C]
 | [\u0B0F-\u0B10]
 | [\u0B13-\u0B28]
 | [\u0B2A-\u0B30]
 | [\u0B32-\u0B33]
 | [\u0B36-\u0B39]
 | [\u0B3D]
 | [\u0B5C-\u0B5D]
 | [\u0B5F-\u0B61]
 | [\u0B85-\u0B8A]
 | [\u0B8E-\u0B90]
 | [\u0B92-\u0B95]
 | [\u0B99-\u0B9A]
 | [\u0B9C]
 | [\u0B9E-\u0B9F]
 | [\u0BA3-\u0BA4]
 | [\u0BA8-\u0BAA]
 | [\u0BAE-\u0BB5]
 | [\u0BB7-\u0BB9]
 | [\u0C05-\u0C0C]
 | [\u0C0E-\u0C10]
 | [\u0C12-\u0C28]
 | [\u0C2A-\u0C33]
 | [\u0C35-\u0C39]
 | [\u0C60-\u0C61]
 | [\u0C85-\u0C8C]
 | [\u0C8E-\u0C90]
 | [\u0C92-\u0CA8]
 | [\u0CAA-\u0CB3]
 | [\u0CB5-\u0CB9]
 | [\u0CDE]
 | [\u0CE0-\u0CE1]
 | [\u0D05-\u0D0C]
 | [\u0D0E-\u0D10]
 | [\u0D12-\u0D28]
 | [\u0D2A-\u0D39]
 | [\u0D60-\u0D61]
 | [\u0D85-\u0D96]
 | [\u0D9A-\u0DB1]
 | [\u0DB3-\u0DBB]
 | [\u0DBD]
 | [\u0DC0-\u0DC6]
 | [\u0E01-\u0E30]
 | [\u0E32-\u0E33]
 | [\u0E40-\u0E46]
 | [\u0E81-\u0E82]
 | [\u0E84]
 | [\u0E87-\u0E88]
 | [\u0E8A]
 | [\u0E8D]
 | [\u0E94-\u0E97]
 | [\u0E99-\u0E9F]
 | [\u0EA1-\u0EA3]
 | [\u0EA5]
 | [\u0EA7]
 | [\u0EAA-\u0EAB]
 | [\u0EAD-\u0EB0]
 | [\u0EB2-\u0EB3]
 | [\u0EBD-\u0EC4]
 | [\u0EC6]
 | [\u0EDC-\u0EDD]
 | [\u0F00]
 | [\u0F40-\u0F6A]
 | [\u0F88-\u0F8B]
 | [\u1000-\u1021]
 | [\u1023-\u1027]
 | [\u1029-\u102A]
 | [\u1050-\u1055]
 | [\u10A0-\u10C5]
 | [\u10D0-\u10F6]
 | [\u1100-\u1159]
 | [\u115F-\u11A2]
 | [\u11A8-\u11F9]
 | [\u1200-\u1206]
 | [\u1208-\u1246]
 | [\u1248]
 | [\u124A-\u124D]
 | [\u1250-\u1256]
 | [\u1258]
 | [\u125A-\u125D]
 | [\u1260-\u1286]
 | [\u1288]
 | [\u128A-\u128D]
 | [\u1290-\u12AE]
 | [\u12B0]
 | [\u12B2-\u12B5]
 | [\u12B8-\u12BE]
 | [\u12C0]
 | [\u12C2-\u12C5]
 | [\u12C8-\u12CE]
 | [\u12D0-\u12D6]
 | [\u12D8-\u12EE]
 | [\u12F0-\u130E]
 | [\u1310]
 | [\u1312-\u1315]
 | [\u1318-\u131E]
 | [\u1320-\u1346]
 | [\u1348-\u135A]
 | [\u13A0-\u13B0]
 | [\u13B1-\u13F4]
 | [\u1401-\u1676]
 | [\u1681-\u169A]
 | [\u16A0-\u16EA]
 | [\u1780-\u17B3]
 | [\u1820-\u1877]
 | [\u1880-\u18A8]
 | [\u1E00-\u1E9B]
 | [\u1EA0-\u1EE0]
 | [\u1EE1-\u1EF9]
 | [\u1F00-\u1F15]
 | [\u1F18-\u1F1D]
 | [\u1F20-\u1F39]
 | [\u1F3A-\u1F45]
 | [\u1F48-\u1F4D]
 | [\u1F50-\u1F57]
 | [\u1F59]
 | [\u1F5B]
 | [\u1F5D]
 | [\u1F5F-\u1F7D]
 | [\u1F80-\u1FB4]
 | [\u1FB6-\u1FBC]
 | [\u1FBE]
 | [\u1FC2-\u1FC4]
 | [\u1FC6-\u1FCC]
 | [\u1FD0-\u1FD3]
 | [\u1FD6-\u1FDB]
 | [\u1FE0-\u1FEC]
 | [\u1FF2-\u1FF4]
 | [\u1FF6-\u1FFC]
 | [\u207F]
 | [\u2102]
 | [\u2107]
 | [\u210A-\u2113]
 | [\u2115]
 | [\u2119-\u211D]
 | [\u2124]
 | [\u2126]
 | [\u2128]
 | [\u212A-\u212D]
 | [\u212F-\u2131]
 | [\u2133-\u2139]
 | [\u2160-\u2183]
 | [\u3005-\u3007]
 | [\u3021-\u3029]
 | [\u3031-\u3035]
 | [\u3038-\u303A]
 | [\u3041-\u3094]
 | [\u309D-\u309E]
 | [\u30A1-\u30FA]
 | [\u30FC-\u30FE]
 | [\u3105-\u312C]
 | [\u3131-\u318E]
 | [\u31A0-\u31B7]
 | [\u3400]
 | [\u4DB5]
 | [\u4E00]
 | [\u9FA5]
 | [\uA000-\uA48C]
 | [\uAC00]
 | [\uD7A3]
 | [\uF900-\uFA2D]
 | [\uFB00-\uFB06]
 | [\uFB13-\uFB17]
 | [\uFB1D]
 | [\uFB1F-\uFB28]
 | [\uFB2A-\uFB36]
 | [\uFB38-\uFB3C]
 | [\uFB3E]
 | [\uFB40-\uFB41]
 | [\uFB43-\uFB44]
 | [\uFB46-\uFBB1]
 | [\uFBD3-\uFD3D]
 | [\uFD50-\uFD8F]
 | [\uFD92-\uFDC7]
 | [\uFDF0-\uFDFB]
 | [\uFE70-\uFE72]
 | [\uFE74]
 | [\uFE76-\uFEFC]
 | [\uFF21-\uFF3A]
 | [\uFF41-\uFF5A]
 | [\uFF66-\uFFBE]
 | [\uFFC2-\uFFC7]
 | [\uFFCA-\uFFCF]
 | [\uFFD2-\uFFD7]
 | [\uFFDA-\uFFDC]
 ;

fragment UnicodeCombiningMark
 : [\u0300-\u034E]
 | [\u0360-\u0362]
 | [\u0483-\u0486]
 | [\u0591-\u05A1]
 | [\u05A3-\u05B9]
 | [\u05BB-\u05BD]
 | [\u05BF] 
 | [\u05C1-\u05C2]
 | [\u05C4]
 | [\u064B-\u0655]
 | [\u0670]
 | [\u06D6-\u06DC]
 | [\u06DF-\u06E4]
 | [\u06E7-\u06E8]
 | [\u06EA-\u06ED]
 | [\u0711]
 | [\u0730-\u074A]
 | [\u07A6-\u07B0]
 | [\u0901-\u0903]
 | [\u093C]
 | [\u093E-\u094D]
 | [\u0951-\u0954]
 | [\u0962-\u0963]
 | [\u0981-\u0983]
 | [\u09BC-\u09C4]
 | [\u09C7-\u09C8]
 | [\u09CB-\u09CD]
 | [\u09D7]
 | [\u09E2-\u09E3]
 | [\u0A02]
 | [\u0A3C]
 | [\u0A3E-\u0A42]
 | [\u0A47-\u0A48]
 | [\u0A4B-\u0A4D]
 | [\u0A70-\u0A71]
 | [\u0A81-\u0A83]
 | [\u0ABC]
 | [\u0ABE-\u0AC5]
 | [\u0AC7-\u0AC9]
 | [\u0ACB-\u0ACD]
 | [\u0B01-\u0B03]
 | [\u0B3C]
 | [\u0B3E-\u0B43]
 | [\u0B47-\u0B48]
 | [\u0B4B-\u0B4D]
 | [\u0B56-\u0B57]
 | [\u0B82-\u0B83]
 | [\u0BBE-\u0BC2]
 | [\u0BC6-\u0BC8]
 | [\u0BCA-\u0BCD]
 | [\u0BD7]
 | [\u0C01-\u0C03]
 | [\u0C3E-\u0C44]
 | [\u0C46-\u0C48]
 | [\u0C4A-\u0C4D]
 | [\u0C55-\u0C56]
 | [\u0C82-\u0C83]
 | [\u0CBE-\u0CC4]
 | [\u0CC6-\u0CC8]
 | [\u0CCA-\u0CCD]
 | [\u0CD5-\u0CD6]
 | [\u0D02-\u0D03]
 | [\u0D3E-\u0D43]
 | [\u0D46-\u0D48]
 | [\u0D4A-\u0D4D]
 | [\u0D57]
 | [\u0D82-\u0D83]
 | [\u0DCA]
 | [\u0DCF-\u0DD4]
 | [\u0DD6]
 | [\u0DD8-\u0DDF]
 | [\u0DF2-\u0DF3]
 | [\u0E31]
 | [\u0E34-\u0E3A]
 | [\u0E47-\u0E4E]
 | [\u0EB1]
 | [\u0EB4-\u0EB9]
 | [\u0EBB-\u0EBC]
 | [\u0EC8-\u0ECD]
 | [\u0F18-\u0F19]
 | [\u0F35]
 | [\u0F37]
 | [\u0F39]
 | [\u0F3E-\u0F3F]
 | [\u0F71-\u0F84]
 | [\u0F86-\u0F87]
 | [\u0F90-\u0F97]
 | [\u0F99-\u0FBC]
 | [\u0FC6]
 | [\u102C-\u1032]
 | [\u1036-\u1039]
 | [\u1056-\u1059]
 | [\u17B4-\u17D3]
 | [\u18A9]
 | [\u20D0-\u20DC]
 | [\u20E1]
 | [\u302A-\u302F]
 | [\u3099-\u309A]
 | [\uFB1E]
 | [\uFE20-\uFE23]
 ;

fragment UnicodeDigit
 : [\u0030-\u0039]
 | [\u0660-\u0669]
 | [\u06F0-\u06F9]
 | [\u0966-\u096F]
 | [\u09E6-\u09EF]
 | [\u0A66-\u0A6F]
 | [\u0AE6-\u0AEF]
 | [\u0B66-\u0B6F]
 | [\u0BE7-\u0BEF]
 | [\u0C66-\u0C6F]
 | [\u0CE6-\u0CEF]
 | [\u0D66-\u0D6F]
 | [\u0E50-\u0E59]
 | [\u0ED0-\u0ED9]
 | [\u0F20-\u0F29]
 | [\u1040-\u1049]
 | [\u1369-\u1371]
 | [\u17E0-\u17E9]
 | [\u1810-\u1819]
 | [\uFF10-\uFF19]
 ;

fragment UnicodeConnectorPunctuation
 : [\u005F]
 | [\u203F-\u2040]
 | [\u30FB]
 | [\uFE33-\uFE34]
 | [\uFE4D-\uFE4F]
 | [\uFF3F]
 | [\uFF65]
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
