import { Parser, TokenStream, BufferedTokenStream, Token } from 'antlr4';
import GoLexer from './GoLexer.js';
import GoParser, { ImportSpecContext } from './GoParser.js';

export default abstract class GoParserBase extends Parser {

    debug: boolean;
    table: Set<string>;

    constructor(input: TokenStream) {
        super(input);
    }

    protected myreset(): void {
        this.debug = false;
        this.table = new Set<string>();
    }

    protected closingBracket(): boolean {
        const stream = this._input as BufferedTokenStream;
        const la = stream.LT(1);
        return la.type === GoParser.R_CURLY || la.type === GoParser.R_PAREN || la.type === Token.EOF;
    }

    public isNotReceive(): boolean {
        const stream = this._input as BufferedTokenStream;
        const la = stream.LT(2);
        return la.type !== GoParser.RECEIVE;
    }

    public addImportSpec(): void {
        const ctx = this._ctx;
        const count = ctx.getChildCount();
        if (!(ctx instanceof ImportSpecContext)) {
            return;
        }
        const importSpec = ctx;
        var packageName = importSpec.packageName();
        if (packageName != null) {
            var name = packageName.getText();
            if (this.debug) console.log("Entering " + name);
            this.table.add(name);
        }
        else {
            var name = importSpec.importPath().getText();
            name = name.replace("\"", "");
            name = name.replace("\"", "");
            name = name.replace("\\", "/");
            const pathArr = name.split('/');
            const fileArr = pathArr.at(-1).split('.');
            const fileName = fileArr.at(-1).toString();
            if (this.debug) console.log("Entering " + fileName);
            this.table.add(fileName);
        }
    }

    protected isType(): boolean {
        const stream = this._input as BufferedTokenStream;
        const la = stream.LA(1);
        return la !== GoParser.IDENTIFIER;
    }

    public isOperand(): boolean {
        const stream = this._input as BufferedTokenStream;
        const la = stream.LT(1);
        if (la.text === "err") return true;
        var result = true;
        if (la.type !== GoParser.IDENTIFIER) {
            if (this.debug) console.log("isOperand Returning " + result + " for " + la);
            return result;
        }
        result = this.table.has(la.text);
        var la2 = stream.LT(2);
        // If it's not followed by a '.', then it really should be
        // considered as operand.
        if (la2.type !== GoParser.DOT) {
            result = true;
            if (this.debug) console.log("isOperand Returning " + result + " for " + la);
            return result;
        }
        // If it's followed by '.', and then followed by '(', then
        // it is a typeAssertion, and so la must be an operand.
        var la3 = stream.LT(3);
        if (la3.type === GoParser.L_PAREN) {
            result = true;
            if (this.debug) console.log("isOperand Returning " + result + " for " + la);
            return result;
        }
        if (this.debug) console.log("isOperand Returning " + result + " for " + la);
        return result;
    }

    public isConversion(): boolean {
        const stream = this._input as BufferedTokenStream;
        const la = stream.LT(1);
        var result = la.type !== GoParser.IDENTIFIER;
        if (this.debug) console.log("isConversion Returning " + result + " for " + la);
        return result;
    }

    public isMethodExpr(): boolean {
        const stream = this._input as BufferedTokenStream;
        const la = stream.LT(1);
        var result = true;
        // See if it looks like a method expr.
        if (la.type === GoParser.STAR) {
            if (this.debug) console.log("isMethodExpr Returning " + result + " for " + la);
            return result;
        }
        if (la.type !== GoParser.IDENTIFIER) {
            result = false;
            if (this.debug) console.log("isMethodExpr Returning " + result + " for " + la);
            return result;
        }
        result = !this.table.has(la.text);
        if (this.debug) console.log("isMethodExpr Returning " + result + " for " + la);
        return result;
    }

    // Built-in functions that take a type as first argument
    private static readonly BUILTIN_TYPE_FUNCTIONS: Set<string> = new Set(["make", "new"]);

    // Check if we're in a call to a built-in function that takes a type as first argument.
    // Called after L_PAREN has been matched in the arguments rule.
    public isTypeArgument(): boolean {
        const stream = this._input as BufferedTokenStream;
        // After matching L_PAREN, LT(-1) is '(' and LT(-2) is the token before it
        const funcToken = stream.LT(-2);
        if (funcToken === null || funcToken.type !== GoParser.IDENTIFIER) {
            if (this.debug) console.log("isTypeArgument Returning false - no identifier before (");
            return false;
        }
        const result = GoParserBase.BUILTIN_TYPE_FUNCTIONS.has(funcToken.text);
        if (this.debug) console.log("isTypeArgument Returning " + result + " for " + funcToken.text);
        return result;
    }

    // Check if we're NOT in a call to a built-in function that takes a type.
    // This is the inverse of isTypeArgument for the expressionList alternative.
    public isExpressionArgument(): boolean {
        const result = !this.isTypeArgument();
        if (this.debug) console.log("isExpressionArgument Returning " + result);
        return result;
    }
}
