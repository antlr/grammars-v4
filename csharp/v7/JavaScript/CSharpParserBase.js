import antlr4 from 'antlr4';
import CSharpParser from './CSharpParser.js';

const ALL_SEMANTIC_FUNCTIONS = ["IsLocalVariableDeclaration"];

function parseNoSemantics(args) {
    const result = new Set();
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

export default class CSharpParserBase extends antlr4.Parser {
    constructor(input) {
        super(input);
        this._noSemantics = parseNoSemantics(process.argv);
    }

    IsLocalVariableDeclaration() {
        if (this._noSemantics.has("IsLocalVariableDeclaration")) return true;
        const local_var_decl = this._ctx;
        if (!(local_var_decl instanceof CSharpParser.Local_variable_declarationContext)) return true;
        if (local_var_decl == null) return true;
        const local_variable_type = local_var_decl.local_variable_type();
        if (local_variable_type == null) return true;
        if (local_variable_type.getText() === "var") return false;
        return true;
    }
}
