import { Symbol } from './Symbol.js';
import { TypeClassification } from './TypeClassification.js';

export class SymbolTable {
    private scopeStack: Symbol[] = [];
    private blockCounter: number = 0;

    constructor() {
        const globalScope = new Symbol("global", new Set([TypeClassification.Global_]));
        this.scopeStack.push(globalScope);

        // Predefined scalar types
        for (const name of ["integer", "natural", "positive", "float", "long_float",
                            "character", "wide_character", "wide_wide_character", "boolean", "duration"]) {
            this.defineType(name, false);
        }
        // Predefined composite types
        for (const name of ["string", "wide_string", "wide_wide_string"]) {
            this.defineType(name, true);
        }
        // Predefined exceptions
        for (const name of ["constraint_error", "program_error", "storage_error", "tasking_error", "numeric_error"]) {
            this.definePredefined(name, TypeClassification.ExceptionName_);
        }
        // Predefined enumeration literals
        this.definePredefined("true", TypeClassification.EnumerationLiteral_);
        this.definePredefined("false", TypeClassification.EnumerationLiteral_);
    }

    private defineType(name: string, isComposite: boolean): void {
        const s = new Symbol(name, new Set([TypeClassification.TypeName_]));
        s.predefined = true;
        s.isComposite = isComposite;
        this.define(s);
    }

    private definePredefined(name: string, tc: TypeClassification): void {
        const s = new Symbol(name, new Set([tc]));
        s.predefined = true;
        this.define(s);
    }

    currentScope(): Symbol | null {
        if (this.scopeStack.length === 0) return null;
        return this.scopeStack[this.scopeStack.length - 1];
    }

    define(symbol: Symbol): boolean {
        const current = this.currentScope();
        if (current === null) return false;
        symbol.name = symbol.name.toLowerCase();
        if (current.members.has(symbol.name)) return false;
        symbol.parent = current;
        current.members.set(symbol.name, symbol);
        return true;
    }

    resolve(name: string): Symbol | null {
        const lower = name.toLowerCase();
        for (let i = this.scopeStack.length - 1; i >= 0; i--) {
            const sym = this.scopeStack[i].members.get(lower);
            if (sym !== undefined) return sym;
        }
        return null;
    }

    pushBlockScope(): Symbol {
        this.blockCounter++;
        const blockScope = new Symbol("block" + this.blockCounter, new Set([TypeClassification.Block_]));
        blockScope.predefined = true;
        this.scopeStack.push(blockScope);
        return blockScope;
    }

    popBlockScope(): void {
        if (this.scopeStack.length <= 1) throw new Error("SymbolTable: scope stack underflow");
        this.scopeStack.pop();
    }

    getExportedSymbols(): Symbol[] {
        const result: Symbol[] = [];
        if (this.scopeStack.length === 0) return result;
        const global = this.scopeStack[0];
        for (const [, sym] of global.members) {
            if (!sym.predefined) {
                result.push(sym);
            }
        }
        return result;
    }

    toString(): string {
        let sb = "";
        if (this.scopeStack.length > 0) {
            sb = this.toStringHelper(sb, this.scopeStack[0], 0);
        }
        return sb;
    }

    private toStringHelper(sb: string, scope: Symbol, depth: number): string {
        const indent = "  ".repeat(depth);
        for (const [, sym] of scope.members) {
            if (!sym.predefined) {
                sb += indent + sym.toString() + "\n";
            }
            if (sym.classification.has(TypeClassification.Block_) ||
                sym.classification.has(TypeClassification.SubprogramName_) ||
                sym.classification.has(TypeClassification.PackageName_)) {
                sb = this.toStringHelper(sb, sym, depth + 1);
            }
        }
        return sb;
    }
}
