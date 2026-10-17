import { Parser, TokenStream } from "antlr4";
import CSharpParser, { Local_variable_declarationContext } from "./CSharpParser.js";

const ALL_SEMANTIC_FUNCTIONS = ["IsLocalVariableDeclaration"];

function parseNoSemantics(args: string[]): Set<string> {
    const result = new Set<string>();
    for (const a of args) {
        if (a.toLowerCase().startsWith("--no-semantics")) {
            const eq = a.indexOf('=');
            if (eq === -1) {
                for (const f of ALL_SEMANTIC_FUNCTIONS) result.add(f);
            } else {
                for (const f of a.substring(eq + 1).split(',')) result.add(f.trim());
            }
        }
    }
    return result;
}

export default abstract class CSharpParserBase extends Parser {
    private _noSemantics: Set<string>;

    constructor(input: TokenStream) {
        super(input);
        this._noSemantics = parseNoSemantics(process.argv);
    }

    protected IsRightArrow(): boolean { return this.areAdjacent(); }
    protected IsRightShift(): boolean { return this.areAdjacent(); }
    protected IsRightShiftAssignment(): boolean { return this.areAdjacent(); }

    private areAdjacent(): boolean {
        const first  = this._input.LT(-2);
        const second = this._input.LT(-1);
        return first !== null && second !== null &&
               first.tokenIndex + 1 === second.tokenIndex;
    }

    protected IsLocalVariableDeclaration(): boolean {
        if (this._noSemantics.has("IsLocalVariableDeclaration")) return true;
        const local_var_decl = this._ctx as Local_variable_declarationContext | null;
        if (local_var_decl == null) return true;
        const local_variable_type = local_var_decl.local_variable_type();
        if (local_variable_type == null) return true;
        if (local_variable_type.getText() === "var") return false;
        return true;
    }
}
