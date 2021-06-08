/*
 BSD License

 Copyright (c) 2018, Martin Mirchev All rights reserved.

 Redistribution and use in source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this list of conditions
 and the following disclaimer. 2. Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the documentation and/or other
 materials provided with the distribution. 3. Neither the name of Tom Everett nor the names of its
 contributors may be used to endorse or promote products derived from this software without specific
 prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
 CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

grammar edn;
program: value* EOF;
value:
	NilLiteral
	| BooleanLiteral
	| StringLiteral
	| IntegerLiteral
	| FloatingPointLiteral
	| CharacterLiteral
	| keyword
	| Symbol
	| tag
	| list_
	| vector
	| map_
	| set_;

//maybe add more tags here? did not get a lot of info from the spec
tag: Hash Symbol;
keyword:
	Colon (
		Symbol
		| IntegerLiteral
		| FloatingPointLiteral
		| NilLiteral
		| BooleanLiteral
		// maybe a character literal here?
	);
list_: LeftParenthesis value* RightParenthesis;
vector: LeftBracket value* RightBracket;
map_: LeftBrace (value value)* RightBrace;
set_: HashedLeftBrace value* RightBrace;

NilLiteral: 'nil';

BooleanLiteral: 'true' | 'false';

StringLiteral:
	'"' (
		~["\\]
		| (
			'\\' (
				[ctrn\\"]
				| [bf] //
				| UnicodeCharacter
			)
		)
	)* '"';

IntegerLiteral: Int 'N'?;

FloatingPointLiteral: Int ('M' | FractionalPart? Exponent?);

CharacterLiteral:
	'\\' (
		'newline'
		| 'return'
		| 'space'
		| 'tab'
		| 'backspace'
		| 'formfeed'
		| UnicodeCharacter
		| ~[ \r\n\t]
	);

LeftParenthesis: '(';
RightParenthesis: ')';
LeftBracket: '[';
RightBracket: ']';
LeftBrace: '{';
RightBrace: '}';
HashedLeftBrace: '#{';
Hash: '#';
Colon: ':';

FractionalPart: '.' Digit+;
fragment Exponent: E Digit+;
fragment E: [eE] [+-]?;
Int: [+-]? ('0' | [1-9] Digit*);
fragment Digit: [0-9];
fragment HexDigit: [a-fA-F0-9];
fragment UnicodeCharacter:
	'u' HexDigit HexDigit HexDigit HexDigit;

Symbol: '/' | Name ('/' Name)?;

Name: (( [-+.] (Alpha | Extra | Special)) | (Alpha | Extra)) (
		Alpha
		| Numeric
		| Extra
		| Special
	)*;
fragment Alpha: [a-zA-Z\p{L}];
fragment Numeric: [0-9];
fragment Extra: [.*+!\-_?$%&=<>];
fragment Special: [#:];
Comment: ';' .*? '\r'? '\n' -> skip;
Whitespace: [ ,\r\n\t\u000C] -> skip;