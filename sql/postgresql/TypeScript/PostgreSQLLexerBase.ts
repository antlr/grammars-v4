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

import { CommonToken, Lexer, CharStream, Token, CommonTokenStream } from "antlr4";
import PostgreSQLLexer from './PostgreSQLLexer';


interface IStack<T> {
  push(item: T): void;
  pop(): T | undefined;
  peek(): T | undefined;
  size(): number;
}

class Stack<T> implements IStack<T> {
  private storage: T[] = [];

  constructor(private capacity: number = Infinity) {}

  push(item: T): void {
    if (this.size() === this.capacity) {
      throw Error("Stack has reached max capacity, you cannot add more items");
    }
    this.storage.push(item);
  }

  pop(): T | undefined {
    return this.storage.pop();
  }

  peek(): T | undefined {
    return this.storage[this.size() - 1];
  }

  size(): number {
    return this.storage.length;
  }
}


export default abstract class PostgreSQLLexerBase extends Lexer {
    protected tags: Stack<string> = new Stack();

    constructor(input: CharStream) {
        super(input);
    }

    public PushTag(): void {
        this.tags.push(this.text);
    }

    public IsTag(): boolean {
        return this.text === this.tags.peek();
    }

    public PopTag(): void {
	this.tags.pop();
    }

    public UnterminatedBlockCommentDebugAssert(): void {
        console.assert(this._input.LA(1) === -1 /*EOF*/);
    }

    public CheckLaMinus(): boolean {
        return this._input.LA(1) !== '-'.charCodeAt(0);
    }

    public CheckLaStar(): boolean {
        return this._input.LA(1) !== '*'.charCodeAt(0);
    }

    public CharIsLetter(): boolean {
        return /^[a-zA-Z]$/.test(String.fromCharCode(this._input.LA(-1)));
    }

    public HandleNumericFail(): void {
        this._input.seek(this._input.index - 2);
        // Assuming PostgreSQLLexer.Integral to be an enum or token type
        this._type = PostgreSQLLexer.Integral; // Adjust as per actual token type definition
    }

    public HandleLessLessGreaterGreater(): void {
        if (this.text.toString() === "<<") this._type = PostgreSQLLexer.LESS_LESS;
        if (this.text.toString() === ">>") this._type = PostgreSQLLexer.GREATER_GREATER;
    }

    public CheckIfUtf32Letter(): boolean {
        const highSurrogate = String.fromCharCode(this._input.LA(-2));
        const lowSurrogate = String.fromCharCode(this._input.LA(-1));
        const combined = highSurrogate + lowSurrogate;
        return /^[a-zA-Z]$/.test(combined);
    }

    public IsSemiColon(): boolean {
        return ';' === String.fromCharCode(this._input.LA(1));
    }
}
