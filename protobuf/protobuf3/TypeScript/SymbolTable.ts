import { Symbol } from './Symbol.js';
import { TypeClassification } from './TypeClassification.js';

export class SymbolTable {
    private scopeStack: Symbol[] = [];

    constructor() {
        const globalScope = new Symbol("global", TypeClassification.Package_);
        this.scopeStack.push(globalScope);
    }

    enterScope(newScope: Symbol): void {
        const current = this.peek();
        if (newScope === current) return;
        this.scopeStack.push(newScope);
    }

    exitScope(): void {
        const current = this.peek();
        this.scopeStack.pop();
        if (this.scopeStack.length === 0) {
            throw new Error("Scope stack is empty");
        }
    }

    currentScope(): Symbol | null {
        if (this.scopeStack.length === 0) return null;
        return this.peek();
    }

    define(symbol: Symbol): boolean {
        const currentScope = this.currentScope();
        return this.defineInScope(currentScope, symbol);
    }

    defineInScope(currentScope: Symbol | null, symbol: Symbol): boolean {
        if (!currentScope) return false;
        if (currentScope.members.has(symbol.name)) {
            return false; // Symbol already defined in current scope
        }
        symbol.parent = currentScope;
        currentScope.members.set(symbol.name, symbol);
        return true;
    }

    resolve(name: string, startScope?: Symbol): Symbol | null {
        if (!startScope) {
            for (let i = this.scopeStack.length - 1; i >= 0; i--) {
                const scope = this.scopeStack[i];
                const symbol = scope.members.get(name);
                if (symbol) return symbol;
            }
            return null;
        } else {
            const symbol = startScope.members.get(name);
            return symbol ?? null;
        }
    }

    toString(): string {
        let sb: string[] = [];
        for (const scope of this.scopeStack) {
            for (const [_, member] of scope.members) {
                sb.push(member.toString());
            }
        }
        return sb.join("\n");
    }

    private peek(): Symbol {
        return this.scopeStack[this.scopeStack.length - 1];
    }
}
