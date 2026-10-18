/*
PostgreSQL grammar.
The MIT License (MIT).
Copyright (c) 2021-2023, Oleksii Kovalov (Oleksii.Kovalov@outlook.com).
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

import antlr4 from 'antlr4';
import PostgreSQLLexer from './PostgreSQLLexer.js';

class Stack {
	constructor(capacity = Infinity) {
		this.capacity = capacity;
		this.storage = [];
	}

	push(item) {
		if (this.size() === this.capacity) {
			throw Error("Stack has reached max capacity, you cannot add more items");
		}
		this.storage.push(item);
	}

	pop() {
		return this.storage.pop();
	}

	peek() {
		return this.storage[this.size() - 1];
	}

	size() {
		return this.storage.length;
	}
}

export default class PostgreSQLLexerBase extends antlr4.Lexer {
	constructor(input) {
		super(input);
		this.tags = new Stack();
	}

	PushTag() {
		this.tags.push(this.text);
	}

	IsTag() {
		return this.text === this.tags.peek();
	}

	PopTag() {
		this.tags.pop();
	}

	UnterminatedBlockCommentDebugAssert() {
		console.assert(this._input.LA(1) === -1); // EOF
	}

	CheckLaMinus() {
		return this._input.LA(1) !== '-'.charCodeAt(0);
	}

	CheckLaStar() {
		return this._input.LA(1) !== '*'.charCodeAt(0);
	}

	CharIsLetter() {
		return /^[a-zA-Z]$/.test(String.fromCharCode(this._input.LA(-1)));
	}

	HandleNumericFail() {
		this._input.seek(this._input.index - 2);
		this._type = PostgreSQLLexer.Integral; // Adjust as per actual token type definition
	}

	HandleLessLessGreaterGreater() {
		if (this.text.toString() === "<<") this._type = PostgreSQLLexer.LESS_LESS;
		if (this.text.toString() === ">>") this._type = PostgreSQLLexer.GREATER_GREATER;
	}

	CheckIfUtf32Letter() {
		const highSurrogate = String.fromCharCode(this._input.LA(-2));
		const lowSurrogate = String.fromCharCode(this._input.LA(-1));
		const combined = highSurrogate + lowSurrogate;
		return /^[a-zA-Z]$/.test(combined);
	}

	IsSemiColon() {
		return ';' === String.fromCharCode(this._input.LA(1));
	}
}
