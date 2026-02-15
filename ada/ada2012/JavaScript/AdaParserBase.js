import antlr4 from 'antlr4';
import AdaLexer from './AdaLexer.js';
import AdaParser from './AdaParser.js';

class SimpleTokenSource {
    constructor(tokens) {
        this.tokens = tokens;
        this.pos = 0;
    }
    nextToken() {
        if (this.pos >= this.tokens.length) {
            return this.tokens[this.tokens.length - 1];
        }
        return this.tokens[this.pos++];
    }
    getSourceName() { return "pragma"; }
}

export default class AdaParserBase extends antlr4.Parser {
    constructor(input) {
        super(input);
    }

    ParsePragmas() {
        var stream = this._input;
        stream.fill();
        var allTokens = stream.tokens;
        var PRAGMA_CHANNEL = 2;
        var currentPragma = null;
        var pragmas = [];
        for (var i = 0; i < allTokens.length; i++) {
            var token = allTokens[i];
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
        for (var j = 0; j < pragmas.length; j++) {
            var pragmaTokens = pragmas[j];
            var defaultChannelTokens = [];
            for (var k = 0; k < pragmaTokens.length; k++) {
                var t = pragmaTokens[k];
                var ct = new antlr4.CommonToken(t.source, t.type, antlr4.Token.DEFAULT_CHANNEL, t.start, t.stop);
                ct.text = t.text;
                ct.line = t.line;
                ct.column = t.column;
                ct.tokenIndex = t.tokenIndex;
                defaultChannelTokens.push(ct);
            }
            var eof = new antlr4.CommonToken([null, null], antlr4.Token.EOF, antlr4.Token.DEFAULT_CHANNEL, -1, -1);
            defaultChannelTokens.push(eof);
            var tokenSource = new SimpleTokenSource(defaultChannelTokens);
            var tokenStream = new antlr4.CommonTokenStream(tokenSource);
            var parser = new AdaParser(tokenStream);
            parser.removeErrorListeners();
            var listeners = this._listeners;
            if (listeners) {
                for (var l = 0; l < listeners.length; l++) {
                    parser.addErrorListener(listeners[l]);
                }
            }
            parser.pragmaRule();
        }
    }
}
