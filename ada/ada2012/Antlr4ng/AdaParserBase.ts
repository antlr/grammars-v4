import { Parser, TokenStream, BufferedTokenStream, Token, CommonToken, CommonTokenStream, ListTokenSource } from "antlr4ng";
import { AdaLexer } from "./AdaLexer.js";
import { AdaParser } from "./AdaParser.js";

export abstract class AdaParserBase extends Parser {
    constructor(input: TokenStream) {
        super(input);
    }

    ParsePragmas(): void {
        const stream = this.inputStream as BufferedTokenStream;
        stream.fill();
        const allTokens = stream.getTokens();
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
                const ct = CommonToken.fromToken(t);
                ct.channel = Token.DEFAULT_CHANNEL;
                defaultChannelTokens.push(ct);
            }
            const eof = CommonToken.fromType(Token.EOF);
            eof.channel = Token.DEFAULT_CHANNEL;
            defaultChannelTokens.push(eof);
            const tokenSource = new ListTokenSource(defaultChannelTokens);
            const tokenStream = new CommonTokenStream(tokenSource);
            const parser = new AdaParser(tokenStream);
            parser.removeErrorListeners();
            for (const listener of this.errorListeners) {
                parser.addErrorListener(listener);
            }
            parser.pragmaRule();
        }
    }
}
