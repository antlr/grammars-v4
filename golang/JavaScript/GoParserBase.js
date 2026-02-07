import { Parser, Token } from 'antlr4';
import GoLexer from './GoLexer.js';
import GoParser from './GoParser.js';

export default class GoParserBase extends Parser {
    constructor(input) {
        super(input);
        this.debug = false;
        this.table = new Set();
    }

    myreset() {
        this.table = new Set();
    }

    closingBracket() {
        const stream = this._input;
        const la = stream.LA(1);
        return la === GoLexer.R_CURLY || la === GoLexer.R_PAREN || la === Token.EOF;
    }

    isType() {
        const stream = this._input;
        const la = stream.LA(1);
        return la !== GoLexer.IDENTIFIER;
    }

    isNotReceive() {
        const stream = this._input;
        const la = stream.LA(2);
        return la !== GoLexer.RECEIVE;
    }

    addImportSpec() {
        // Minimal implementation for JavaScript target
    }

    isOperand() {
        const stream = this._input;
        const la = stream.LT(1);
        if (la.text === "err") return true;
        if (la.type !== GoParser.IDENTIFIER) return true;
        if (this.table.has(la.text)) return true;
        const la2 = stream.LT(2);
        if (la2.type !== GoParser.DOT) return true;
        const la3 = stream.LT(3);
        if (la3.type === GoParser.L_PAREN) return true;
        return this.table.has(la.text);
    }

    isConversion() {
        const stream = this._input;
        const la = stream.LT(1);
        return la.type !== GoParser.IDENTIFIER;
    }

    isMethodExpr() {
        const stream = this._input;
        const la = stream.LT(1);
        if (la.type === GoParser.STAR) return true;
        if (la.type !== GoParser.IDENTIFIER) return false;
        return !this.table.has(la.text);
    }

    // Built-in functions that take a type as first argument
    static BUILTIN_TYPE_FUNCTIONS = new Set(["make", "new"]);

    // Check if we're in a call to a built-in function that takes a type as first argument.
    // Called after L_PAREN has been matched in the arguments rule.
    isTypeArgument() {
        const stream = this._input;
        // After matching L_PAREN, LT(-1) is '(' and LT(-2) is the token before it
        const funcToken = stream.LT(-2);
        if (funcToken === null || funcToken.type !== GoParser.IDENTIFIER) {
            return false;
        }
        return GoParserBase.BUILTIN_TYPE_FUNCTIONS.has(funcToken.text);
    }

    // Check if we're NOT in a call to a built-in function that takes a type.
    // This is the inverse of isTypeArgument for the expressionList alternative.
    isExpressionArgument() {
        return !this.isTypeArgument();
    }
}

