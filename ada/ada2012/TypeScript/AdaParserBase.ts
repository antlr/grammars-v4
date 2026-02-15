import { Parser, TokenStream, BufferedTokenStream, Token, CommonToken, CommonTokenStream, ListTokenSource } from "antlr4";
import AdaLexer from './AdaLexer';
import AdaParser from './AdaParser';

export default abstract class AdaParserBase extends Parser {
    constructor(input: TokenStream) {
        super(input);
    }

    ParsePragmas(): void {
        const stream = this._input as BufferedTokenStream;
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
                const ct = new CommonToken(null, t.type, Token.DEFAULT_CHANNEL, t.start, t.stop);
                ct.text = t.text;
                ct.line = t.line;
                ct.column = t.column;
                ct.tokenIndex = t.tokenIndex;
                defaultChannelTokens.push(ct);
            }
            const eof = new CommonToken(null, Token.EOF, Token.DEFAULT_CHANNEL, -1, -1);
            defaultChannelTokens.push(eof);
            const tokenSource = new CommonTokenStream(new ListTokenSource(defaultChannelTokens));
            const parser = new AdaParser(tokenSource);
            parser.removeErrorListeners();
            const listeners = this._listeners;
            if (listeners) {
                for (const listener of listeners) {
                    parser.addErrorListener(listener);
                }
            }
            parser.pragmaRule();
        }
    }
}
