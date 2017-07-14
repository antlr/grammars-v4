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
parser grammar JavaScriptParser;

options { tokenVocab=JavaScriptLexer; }

@parser::members
{///<summary>Returns <c>true</c> iff on the current index of the parser's
///token stream a token of the given <c>type</c> exists on the
///<c>Hidden</c> channel.</summary>
///<param name="type">the type of the token on the <c>Hidden</c> channel
///to check.</param>
///<returns><c>true</c> iff on the current index of the parser's
///token stream a token of the given <c>type</c> exists on the
///<c>Hidden</c> channel.</returns>
private bool Here(int type) {
	// Get the token ahead of the current index.
	int possibleIndexEosToken = CurrentToken.TokenIndex - 1;
	IToken ahead = _input.Get(possibleIndexEosToken);

	// Check if the token resides on the Hidden channel and if it's of the
	// provided type.
	return ahead.Channel == Lexer.Hidden && ahead.Type == type;
}

///<summary>Returns <c>true</c> iff on the current index of the parser's
///token stream a token exists on the <c>Hidden</c> channel which
///either is a line terminator, or is a multi line comment that
///contains a line terminator.</summary>
///<returns><c>true</c> iff on the current index of the parser's
///token stream a token exists on the <c>Hidden</c> channel which
///either is a line terminator, or is a multi line comment that
///contains a line terminator.</returns>
private bool LineTerminatorAhead() {
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

program
    : sourceElements? EOF
    ;

sourceElement
    : statement
    | functionDeclaration
    ;

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

block
    : '{' statementList? '}'
    ;

statementList
    : statement+
    ;

variableStatement
    : Var variableDeclarationList eos
    ;

variableDeclarationList
    : variableDeclaration (',' variableDeclaration)*
    ;

variableDeclaration
    : Identifier initialiser?
    ;

initialiser
    : '=' singleExpression
    ;

emptyStatement
    : SemiColon
    ;

expressionStatement
    : {_input.La(1) != OpenBrace && _input.La(1) != Function}? expressionSequence eos
    ;

ifStatement
    : If '(' expressionSequence ')' statement (Else statement)?
    ;


iterationStatement
    : Do statement While '(' expressionSequence ')' eos                                                 # DoStatement
    | While '(' expressionSequence ')' statement                                                        # WhileStatement
    | For '(' expressionSequence? ';' expressionSequence? ';' expressionSequence? ')' statement         # ForStatement
    | For '(' Var variableDeclarationList ';' expressionSequence? ';' expressionSequence? ')' statement # ForVarStatement
    | For '(' singleExpression In expressionSequence ')' statement                                      # ForInStatement
    | For '(' Var variableDeclaration In expressionSequence ')' statement                               # ForVarInStatement
    ;

continueStatement
    : Continue ({!Here(LineTerminator)}? Identifier)? eos
    ;

breakStatement
    : Break ({!Here(LineTerminator)}? Identifier)? eos
    ;

returnStatement
    : Return ({!Here(LineTerminator)}? expressionSequence)? eos
    ;

withStatement
    : With '(' expressionSequence ')' statement
    ;

switchStatement
    : Switch '(' expressionSequence ')' caseBlock
    ;

caseBlock
    : '{' caseClauses? (defaultClause caseClauses?)? '}'
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
    : Identifier ':' statement
    ;

throwStatement
    : Throw {!Here(LineTerminator)}? expressionSequence eos
    ;

tryStatement
    : Try block (catchProduction finallyProduction? | finallyProduction)
    ;

catchProduction
    : Catch '(' Identifier ')' block
    ;

finallyProduction
    : Finally block
    ;

debuggerStatement
    : Debugger eos
    ;

functionDeclaration
    : Function Identifier '(' formalParameterList? ')' '{' functionBody '}'
    ;

formalParameterList
    : Identifier (',' Identifier)*
    ;

functionBody
    : sourceElements?
    ;

sourceElements
    : sourceElement+
    ;

arrayLiteral
    : '[' elementList? ','? elision? ']'
    ;

elementList
    : elision? singleExpression (',' elision? singleExpression)*
    ;

elision
    : ','+
    ;

objectLiteral
    : '{' (propertyAssignment (',' propertyAssignment)*)? ','? '}'
    ;

propertyAssignment
    : propertyName ':' singleExpression              # PropertyExpressionAssignment
    | getter '(' ')' '{' functionBody '}'            # PropertyGetter
    | setter '(' Identifier ')' '{' functionBody '}' # PropertySetter
    ;

propertyName
    : identifierName
    | StringLiteral
    | numericLiteral
    ;

arguments
    : '(' (singleExpression (',' singleExpression)*)? ')'
    ;

expressionSequence
    : singleExpression (',' singleExpression)*
    ;

singleExpression
    : Function Identifier? '(' formalParameterList? ')' '{' functionBody '}' # FunctionExpression
    | singleExpression '[' expressionSequence ']'                            # MemberIndexExpression
    | singleExpression '.' identifierName                                    # MemberDotExpression
    | singleExpression arguments                                             # ArgumentsExpression
    | New singleExpression arguments?                                        # NewExpression
    | singleExpression {!Here(LineTerminator)}? '++'                         # PostIncrementExpression
    | singleExpression {!Here(LineTerminator)}? '--'                         # PostDecreaseExpression
    | Delete singleExpression                                                # DeleteExpression
    | Void singleExpression                                                  # VoidExpression
    | Typeof singleExpression                                                # TypeofExpression
    | '++' singleExpression                                                  # PreIncrementExpression
    | '--' singleExpression                                                  # PreDecreaseExpression
    | '+' singleExpression                                                   # UnaryPlusExpression
    | '-' singleExpression                                                   # UnaryMinusExpression
    | '~' singleExpression                                                   # BitNotExpression
    | '!' singleExpression                                                   # NotExpression
    | singleExpression ('*' | '/' | '%') singleExpression                    # MultiplicativeExpression
    | singleExpression ('+' | '-') singleExpression                          # AdditiveExpression
    | singleExpression ('<<' | '>>' | '>>>') singleExpression                # BitShiftExpression
    | singleExpression ('<' | '>' | '<=' | '>=') singleExpression            # RelationalExpression
    | singleExpression Instanceof singleExpression                           # InstanceofExpression
    | singleExpression In singleExpression                                   # InExpression
    | singleExpression ('==' | '!=' | '===' | '!==') singleExpression        # EqualityExpression
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
    : NullLiteral
    | BooleanLiteral
    | StringLiteral
    | RegularExpressionLiteral
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
    | {LineTerminatorAhead()}?
    | {_input.Lt(1).Type == CloseBrace}?
    ;