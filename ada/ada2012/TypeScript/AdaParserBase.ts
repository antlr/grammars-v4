import { Parser, TokenStream, Token, CommonToken, CommonTokenStream } from "antlr4";
import AdaLexer from './AdaLexer';
import AdaParser from './AdaParser';

class SimpleTokenSource {
    private tokens: Token[];
    private pos: number;
    constructor(tokens: Token[]) {
        this.tokens = tokens;
        this.pos = 0;
    }
    nextToken(): Token {
        if (this.pos >= this.tokens.length) {
            return this.tokens[this.tokens.length - 1];
        }
        return this.tokens[this.pos++];
    }
    getSourceName(): string { return "pragma"; }
}

export default abstract class AdaParserBase extends Parser {
    constructor(input: TokenStream) {
        super(input);
    }

    ParsePragmas(): void {
        const stream = this._input as CommonTokenStream;
        stream.fill();
        const allTokens = stream.tokens;
        const PRAGMA_CHANNEL = 2;
        let currentPragma: Token[] | null = null;
        const pragmas: Token[][] = [];
        for (const token of allTokens) {
            if (token.channel !== PRAGMA_CHANNEL) continue;
            if (token.type === AdaLexer.PRAGMA) {
                currentPragma = [token];
            } else if (currentPragma !== null) {
                currentPragma.push(token);
                if (token.type === AdaLexer.SEMI) {
                    pragmas.push(currentPragma);
                    currentPragma = null;
                }
            }
        }
        for (const pragmaTokens of pragmas) {
            const defaultChannelTokens: Token[] = [];
            for (const t of pragmaTokens) {
                const ct = new CommonToken((t as any).source, t.type, Token.DEFAULT_CHANNEL, t.start, t.stop);
                ct.text = t.text;
                ct.line = t.line;
                ct.column = t.column;
                ct.tokenIndex = t.tokenIndex;
                defaultChannelTokens.push(ct);
            }
            const eof = new CommonToken([null, null] as any, Token.EOF, Token.DEFAULT_CHANNEL, -1, -1);
            defaultChannelTokens.push(eof);
            const tokenSource = new SimpleTokenSource(defaultChannelTokens);
            const tokenStream = new CommonTokenStream(tokenSource as any);
            const parser = new AdaParser(tokenStream);
            parser.removeErrorListeners();
            parser.pragmaRule();
        }
    }
}
