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

import { CommonToken, Lexer, CharStream, Token, CommonTokenStream } from "antlr4ng";
//import { PostgreSQLLexer } from './PostgreSQLLexer.js';

export abstract class PostgreSQLLexerBase extends Lexer {
//    protected static tags: Queue<string> = new Queue<string>();

    constructor(input: CharStream) {
        super(input);
    }

    public PushTag(): void {
//        PostgreSQLLexerBase.tags.enqueue(this.input.toString());
    }

    public IsTag(): boolean {
    return true;
//        return this.input.toString() === PostgreSQLLexerBase.tags.peek();
    }

    public PopTag(): void {
//        PostgreSQLLexerBase.tags.dequeue();
    }

    public UnterminatedBlockCommentDebugAssert(): void {
//        console.assert(this.inputStream.LA(1) === -1 /*EOF*/);
    }

    public CheckLaPlus(): boolean {
return true;
//        return this.inputStream.LA(1) !== '+'.charCodeAt(0);
    }

    public CheckLaMinus(): boolean {
return true;
//        return this.inputStream.LA(1) !== '-'.charCodeAt(0);
    }

    public CheckLaStar(): boolean {
return true;

//        return this.inputStream.LA(1) !== '*'.charCodeAt(0);
    }

    public CharIsLetter(): boolean {
return true;
//        return /^[a-zA-Z]$/.test(String.fromCharCode(this.inputStream.LA(-1)));
    }

    public HandleNumericFail(): void {
//        this.inputStream.seek(this.inputStream.index - 2);
        // Assuming PostgreSQLLexer.Integral to be an enum or token type
//        this.Type = PostgreSQLLexer.Integral; // Adjust as per actual token type definition
    }

    public HandleLessLessGreaterGreater(): void {
//        if (this.input.toString() === "<<") this.Type = PostgreSQLLexer.LESS_LESS;
//        if (this.input.toString() === ">>") this.Type = PostgreSQLLexer.GREATER_GREATER;
    }

    public CheckIfUtf32Letter(): boolean {
return true;
//        const highSurrogate = String.fromCharCode(this.inputStream.LA(-2));
//        const lowSurrogate = String.fromCharCode(this.inputStream.LA(-1));
//        const combined = highSurrogate + lowSurrogate;
//        return /^[a-zA-Z]$/.test(combined);
    }

    public IsSemiColon(): boolean {
return true;
//        return ';' === String.fromCharCode(this.inputStream.LA(1));
    }
}
