import { Parser, TokenStream, BufferedTokenStream, Token } from 'antlr4ng';
import { GoLexer } from './GoLexer.js';
import { GoParser, ImportSpecContext } from './GoParser.js';

export default abstract class GoParserBase extends Parser {

    debug: boolean;
    table: Set<string>;

    constructor(input: TokenStream) {
        super(input);
        this.table = new Set<string>();
        // Check for --debug flag in command line arguments (Node.js environment)
        this.debug = typeof process !== 'undefined' &&
                     process.argv &&
                     process.argv.some(arg => arg.toLowerCase().includes('--debug'));
        if (this.debug) {
            console.log("debug = " + this.debug);
        }
    }

    protected myreset(): void {
        this.table = new Set<string>();
    }

    protected closingBracket(): boolean {
        const stream = this.inputStream as BufferedTokenStream;
        const la = stream.LT(1);
        return la.type === GoParser.R_CURLY || la.type === GoParser.R_PAREN || la.type === Token.EOF;
    }

    public isNotReceive(): boolean
    {
        const stream = this.inputStream as BufferedTokenStream;
        const la = stream.LT(2);
        return la.type !== GoParser.RECEIVE;
    }

    public addImportSpec(): void
    {
        const ctx = this.context;
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
            return;
        }
        const importPath = importSpec.importPath();
        if (importPath == null) {
            return;
        }
        var name = importPath.getText();
        if (this.debug) console.log("import path " + name);
        name = name.replaceAll("\"", "");
        if (name.length === 0) {
            return;
        }
        name = name.replaceAll("\\", "/");
        const pathArr = name.split('/');
        if (pathArr.length === 0) {
            return;
        }
        const lastComponent = pathArr[pathArr.length - 1];
        if (lastComponent.length === 0) {
            return;
        }
        // Handle special cases like "." and ".."
        if (lastComponent === "." || lastComponent === "..") {
            return;
        }
        const fileArr = lastComponent.split('.');
        // Guard against empty array (can happen if lastComponent is all dots)
        if (fileArr.length === 0) {
            this.table.add(lastComponent);
            if (this.debug) console.log("Entering " + lastComponent);
            return;
        }
        var fileName = fileArr[fileArr.length - 1];
        if (fileName.length === 0) {
            // Fall back to lastComponent if split resulted in empty string
            fileName = lastComponent;
        }
        if (this.debug) console.log("Entering " + fileName);
        this.table.add(fileName);
    }

    protected isType(): boolean {
        const stream = this.inputStream as BufferedTokenStream;
        const la = stream.LA(1);
        return la !== GoParser.IDENTIFIER;
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
        var result = la.type !== GoParser.IDENTIFIER;
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

    // Built-in functions that take a type as first argument
    private static readonly BUILTIN_TYPE_FUNCTIONS: Set<string> = new Set(["make", "new"]);

    // Check if we're in a call to a built-in function that takes a type as first argument.
    // Called after L_PAREN has been matched in the arguments rule.
    public isTypeArgument(): boolean
    {
        const stream = this.inputStream as BufferedTokenStream;
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
    public isExpressionArgument(): boolean
    {
        const result = !this.isTypeArgument();
        if (this.debug) console.log("isExpressionArgument Returning " + result);
        return result;
    }
}
