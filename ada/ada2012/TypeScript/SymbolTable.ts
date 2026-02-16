import { Symbol } from './Symbol';
import { TypeClassification } from './TypeClassification';

export class SymbolTable {
    private scopeStack: Symbol[] = [];
    private blockCounter: number = 0;

    constructor() {
        const globalScope = new Symbol("global", new Set([TypeClassification.Global_]));
        this.scopeStack.push(globalScope);

        // Predefined scalar types
        this.defineType("integer", false);
        this.defineType("natural", false);
        this.defineType("positive", false);
        this.defineType("float", false);
        this.defineType("long_float", false);
        this.defineType("character", false);
        this.defineType("wide_character", false);
        this.defineType("wide_wide_character", false);
        this.defineType("boolean", false);
        this.defineType("duration", false);

        // Predefined composite types
        this.defineType("string", true);
        this.defineType("wide_string", true);
        this.defineType("wide_wide_string", true);

        // Predefined exceptions
        const exSym = (name: string) => {
            const s = new Symbol(name, new Set([TypeClassification.ExceptionName_]));
            s.predefined = true;
            this.define(s);
        };
        exSym("constraint_error");
        exSym("program_error");
        exSym("storage_error");
        exSym("tasking_error");
        exSym("numeric_error");

        // Predefined enumeration literals
        const enumSym = (name: string) => {
            const s = new Symbol(name, new Set([TypeClassification.EnumerationLiteral_]));
            s.predefined = true;
            this.define(s);
        };
        enumSym("true");
        enumSym("false");
    }

    private defineType(name: string, isComposite: boolean): void {
        const s = new Symbol(name, new Set([TypeClassification.TypeName_]));
        s.predefined = true;
        s.isComposite = isComposite;
        this.define(s);
    }

    enterScope(newScope: Symbol): void {
        const current = this.currentScope();
        if (newScope === current) return;
        this.scopeStack.push(newScope);
    }

    exitScope(): void {
        this.scopeStack.pop();
        if (this.scopeStack.length === 0) {
            throw new Error("SymbolTable: scope stack underflow");
        }
    }

    currentScope(): Symbol | null {
        if (this.scopeStack.length === 0) return null;
        return this.scopeStack[this.scopeStack.length - 1];
    }

    define(symbol: Symbol): boolean {
        const currentScope = this.currentScope();
        if (currentScope === null) return false;
        return this.defineInScope(currentScope, symbol);
    }

    defineInScope(scope: Symbol, symbol: Symbol): boolean {
        // Case-insensitive: normalize name to lowercase
        symbol.name = symbol.name.toLowerCase();
        if (scope.members.has(symbol.name)) {
            return false;
        }
        symbol.parent = scope;
        scope.members.set(symbol.name, symbol);
        return true;
    }

    resolve(name: string, startScope: Symbol | null = null): Symbol | null {
        // Case-insensitive: normalize to lowercase
        name = name.toLowerCase();
        if (startScope === null) {
            for (let i = this.scopeStack.length - 1; i >= 0; i--) {
                const scope = this.scopeStack[i];
                const symbol = scope.members.get(name);
                if (symbol !== undefined) {
                    return symbol;
                }
            }
            return null;
        } else {
            const symbol = startScope.members.get(name);
            if (symbol !== undefined) {
                return symbol;
            }
            return null;
        }
    }

    pushBlockScope(): Symbol {
        const blockScope = new Symbol("block" + (++this.blockCounter), new Set([TypeClassification.Block_]));
        blockScope.predefined = true;
        this.enterScope(blockScope);
        return blockScope;
    }

    popBlockScope(): void {
        this.exitScope();
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
