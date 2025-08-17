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

import { Parser, TokenStream, CommonTokenStream, Recognizer } from 'antlr4ng';
//import { ParseTree } from 'antlr4ts/tree/ParseTree';
//import { PostgreSQLLexer } from './PostgreSQLLexer'; // Assuming equivalent lexer is available
//import { PostgreSQLParser } from './PostgreSQLParser'; // Assuming equivalent parser is available

export abstract class PostgreSQLParserBase extends Parser {
    constructor(input: TokenStream) {
        super(input);
    }

//    protected getParsedSqlTree(script: string, line: number = 0): ParseTree {
//        const ph = this.getPostgreSQLParser(script);
//        return ph.root(); // Replace with correct start rule
//    }

    protected ParseRoutineBody(): void {
//        const _localctx = this.context as PostgreSQLParser.Createfunc_opt_listContext;
//        const lang = _localctx
//            .createfunc_opt_item()
//            .find(coi => coi.LANGUAGE() !== null)
//            ?.nonreservedword_or_sconst()?.nonreservedword()?.identifier()?.Identifier()?.text;
//
//        const func_as = _localctx.createfunc_opt_item().find(coi => coi.func_as() !== null);
//        if (func_as) {
//            const txt = this.getRoutineBodyString(func_as.func_as().sconst(0));
//            switch (lang) {
//                case 'plpgsql':
//                    // Placeholder for mutating tree.
//                    break;
//                case 'sql':
//                    // Placeholder for mutating tree.
//                    break;
//            }
//        }
    }

    private trimQuotes(s: string): string {
        return s ? s.slice(1, -1) : s;
    }

    private unquote(s: string): string {
        let r = '';
        for (let i = 0; i < s.length; i++) {
            const c = s[i];
            r += c;
            if (c === "'" && i < s.length - 1 && s[i + 1] === "'") i++;
        }
        return r;
    }

/*
    private getRoutineBodyString(rule: PostgreSQLParser.SconstContext): string {
        const anysconst = rule.anysconst();
        const StringConstant = anysconst.StringConstant();
        if (StringConstant) return this.unquote(this.trimQuotes(StringConstant.text));

        const UnicodeEscapeStringConstant = anysconst.UnicodeEscapeStringConstant();
        if (UnicodeEscapeStringConstant) return this.trimQuotes(UnicodeEscapeStringConstant.text);

        const EscapeStringConstant = anysconst.EscapeStringConstant();
        if (EscapeStringConstant) return this.trimQuotes(EscapeStringConstant.text);

        let result = '';
        const dollarText = anysconst.DollarText();
        dollarText.forEach(s => result += s);
        return result;
    }

    private getPostgreSQLParser(script: string): PostgreSQLParser {
        const charStream = CharStreams.fromString(script);
        const lexer = new PostgreSQLLexer(charStream);
        const tokens = new CommonTokenStream(lexer);
        const parser = new PostgreSQLParser(tokens);
        lexer.removeErrorListeners();
        parser.removeErrorListeners();

        // Error listeners are assumed to be available in the environment
        const listenerLexer = new LexerDispatchingErrorListener(tokens.tokenSource as any); // Adjust as needed
        const listenerParser = new ParserDispatchingErrorListener(this);
        lexer.addErrorListener(listenerLexer);
        parser.addErrorListener(listenerParser);
        return parser;
    }
*/
    public OnlyAcceptableOps(): boolean {
        const c = (this.inputStream as CommonTokenStream).LT(1);
        const text = c.text;
        return text === '!' || text === '!!' || text === '!=-'; // Code for specific example
    }
}
