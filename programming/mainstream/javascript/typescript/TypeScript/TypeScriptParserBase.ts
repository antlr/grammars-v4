import {Parser, Lexer, Token, TokenStream} from 'antlr4ts';
import {TypeScriptParser} from './TypeScriptParser';

/**
 * All parser methods that used in grammar (p, prev, notLineTerminator, etc.)
 * should start with lower case char similar to parser rules.
 */
export abstract class TypeScriptParserBase extends Parser {

    constructor(input: TokenStream) {
        super(input);
    }

    /**
     * Short form for prev(String str)
     */
    protected p(str: string):boolean {
        return this.prev(str);
    }

    /**
     * Whether the previous token value equals to @param str
     */
    protected prev(str: string):boolean  {
        return this._input.LT(-1).text === str;
    }

    /**
     * Short form for next(String str)
     */
    protected n(str: string):boolean  {
        return this.next(str);
    }

    /**
     * Whether the next token value equals to @param str
     */
    protected next(str: string):boolean {
        return this._input.LT(1).text === str;
    }

    protected notLineTerminator():boolean {
        return !this.here(TypeScriptParser.LineTerminator);
    }

    protected notOpenBraceAndNotFunction():boolean {
        const nextTokenType:number = this._input.LT(1).type;
        return nextTokenType != TypeScriptParser.OpenBrace && nextTokenType != TypeScriptParser.Function_;
    }

    protected closeBrace():boolean {
        return this._input.LT(1).type == TypeScriptParser.CloseBrace;
    }

    /**
     * Returns {@code true} iff on the current index of the parser's
     * token stream a token of the given {@code type} exists on the
     * {@code HIDDEN} channel.
     *
     * @param  type
     *         the type of the token on the {@code HIDDEN} channel
     *         to check.
     *
     * @return {@code true} iff on the current index of the parser's
     * token stream a token of the given {@code type} exists on the
     * {@code HIDDEN} channel.
     */
    private here(type:number):boolean {

        // Get the token ahead of the current index.
        const possibleIndexEosToken: number = this.currentToken.tokenIndex - 1;
        const ahead: Token = this._input.get(possibleIndexEosToken);

        // Check if the token resides on the HIDDEN channel and if it's of the
        // provided type.
        return (ahead.channel == Lexer.HIDDEN) && (ahead.type == type);
    }

    /**
     * Returns {@code true} iff on the current index of the parser's
     * token stream a token exists on the {@code HIDDEN} channel which
     * either is a line terminator, or is a multi line comment that
     * contains a line terminator.
     *
     * @return {@code true} iff on the current index of the parser's
     * token stream a token exists on the {@code HIDDEN} channel which
     * either is a line terminator, or is a multi line comment that
     * contains a line terminator.
     */
    protected lineTerminatorAhead():boolean {

        // Get the token ahead of the current index.
        let possibleIndexEosToken: number = this.currentToken.tokenIndex - 1;
        let ahead: Token  = this._input.get(possibleIndexEosToken);

        if (ahead.channel != Lexer.HIDDEN) {
            // We're only interested in tokens on the HIDDEN channel.
            return false;
        }

        if (ahead.type == TypeScriptParser.LineTerminator) {
            // There is definitely a line terminator ahead.
            return true;
        }

        if (ahead.type == TypeScriptParser.WhiteSpaces) {
            // Get the token ahead of the current whitespaces.
            possibleIndexEosToken = this.currentToken.tokenIndex - 2;
            ahead = this._input.get(possibleIndexEosToken);
        }

        // Get the token's text and type.
        const text = ahead.text;
        const type = ahead.type;

        // Check if the token is, or contains a line terminator.
        return (type == TypeScriptParser.MultiLineComment && (text.includes("\r") || text.includes("\n"))) ||
                (type == TypeScriptParser.LineTerminator);
    }
}