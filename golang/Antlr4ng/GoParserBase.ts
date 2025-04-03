import { Parser, TokenStream, BufferedTokenStream, Token } from 'antlr4ng';
import { GoLexer } from './GoLexer.js';
import { GoParser, ImportSpecContext } from './GoParser.js';

export default abstract class GoParserBase extends Parser {

    debug: boolean;
    table: Set<string>;

    constructor(input: TokenStream) {
        super(input);
        this.debug = false;
        this.table = new Set<string>();
    }

    protected closingBracket(): boolean {
        const stream = this.inputStream as BufferedTokenStream;
        const la = stream.LT(1);
        return la.type === GoLexer.R_CURLY || la.type === GoLexer.R_PAREN || la.type === Token.EOF;
    }

    public isNotReceive(): boolean
    {
        const stream = this.inputStream as BufferedTokenStream;
        const la = stream.LT(2);
        return la.type !== GoLexer.RECEIVE;
    }

    public addImportSpec(): void
    {
        const ctx = this.context;
        const count = ctx.getChildCount();
        if (!(ctx instanceof ImportSpecContext)) {
            return;
        }
        const importSpec = ctx;
        var packageName = importSpec.packageName();
        if (packageName != null)
        {
            var name = packageName.getText();
            if (this.debug) console.log("Entering " + name);
            this.table.add(name);
        }
        else
        {
            var name = importSpec.importPath().getText();
            name = name.replaceAll("\"", "");
            name = name.replaceAll("\\", "/");
            const pathArr = name.split('/');
            const fileArr = pathArr.at(-1).split('.');
            const fileName = fileArr.at(-1).toString();
            if (this.debug) console.log("Entering " + fileName);
            this.table.add(fileName);
        }
    }

    protected isType(): boolean {
        const stream = this.inputStream as BufferedTokenStream;
        const la = stream.LA(1);
        return la !== GoLexer.IDENTIFIER;
    }

    public isOperand(): boolean
    {
        const stream = this.inputStream as BufferedTokenStream;
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
        if (la3.type === GoParser.L_PAREN)
        {
            result = true;
            if (this.debug) console.log("isOperand Returning " + result + " for " + la);
            return result;
        }
        if (this.debug) console.log("isOperand Returning " + result + " for " + la);
        return result;
    }

    public isConversion(): boolean
    {
        const stream = this.inputStream as BufferedTokenStream;
        const la = stream.LT(1);
        var result = la.type !== GoLexer.IDENTIFIER;
        if (this.debug) console.log("isConversion Returning " + result + " for " + la);
        return result;
    }

    public isMethodExpr(): boolean
    {
        const stream = this.inputStream as BufferedTokenStream;
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
        result = ! this.table.has(la.text);
        if (this.debug) console.log("isMethodExpr Returning " + result + " for " + la);
        return result;
    }
}
