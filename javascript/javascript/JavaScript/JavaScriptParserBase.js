import antlr4 from 'antlr4';
import JavaScriptParser from './JavaScriptParser.js';

export default class JavaScriptParserBase extends antlr4.Parser {

    constructor(input) {
        super(input);
    }

    p(str) {
        return this.prev(str);
    }

    prev(str) {
        return  this._input.LT(-1).text === str;
    }

    // Short form for next(String str)
    n(str)
    {
        return this.next(str);
    }

    // Whether the next token value equals to @param str
    next(str)
    {
        return  this._input.LT(1).text === str;
    }

    notLineTerminator() {
        return !this.here(JavaScriptParser.LineTerminator);
    }

    notOpenBraceAndNotFunction() {
        const nextTokenType = this._input.LT(1).type;
        return (
                nextTokenType !== JavaScriptParser.OpenBrace &&
                nextTokenType !== JavaScriptParser.Function_
               );
    }

    closeBrace() {
        return this._input.LT(1).type === JavaScriptParser.CloseBrace;
    }

    here(type) {
        // Get the most recently emitted token.
        const currentToken = this._input.LT(-1);

        // Get the next token index.
        const nextTokenIndex = currentToken == null ? 0 : currentToken.tokenIndex + 1;

        // Get the token after the `currentToken`. By using `_input.get(index)`,
        // we also grab a token that is (possibly) on the HIDDEN channel.
        const nextToken = this._input.get(nextTokenIndex);

        // Check if the token resides on the HIDDEN channel and if it's of the
        // provided type.
        return nextToken.channel === antlr4.Lexer.HIDDEN && nextToken.type === type;
    }

    lineTerminatorAhead() {
        let possibleIndexEosToken = this.getCurrentToken().tokenIndex - 1;
        if (possibleIndexEosToken < 0) return false;
        let ahead = this._input.get(possibleIndexEosToken);
        if (ahead.channel !== antlr4.Lexer.HIDDEN) {
            return false;
        }
        if (ahead.type === JavaScriptParser.LineTerminator) {
            return true;
        }
        if (ahead.type === JavaScriptParser.WhiteSpaces) {
            possibleIndexEosToken = this.getCurrentToken().tokenIndex - 2;
            if (possibleIndexEosToken < 0) return false;
            ahead = this._input.get(possibleIndexEosToken);
        }
        const text = ahead.text;
        const type = ahead.type;
        return (
                (type === JavaScriptParser.MultiLineComment &&
                 (text.includes("\r") || text.includes("\n"))) ||
                type === JavaScriptParser.LineTerminator
               );
    }
}
