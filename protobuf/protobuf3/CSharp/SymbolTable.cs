using System.Collections.Generic;
using System.Net.NetworkInformation;

public class SymbolTable {
    private Stack<Symbol> scopeStack = new Stack<Symbol>();

    // Note: predeclared identifiers here.

    public SymbolTable() {
        var globalScope = new Symbol() { Name = "global", Classification = TypeClassification.Package_ };
        scopeStack.Push(globalScope);
    }

    public void EnterScope(Symbol newScope) {
        var parent = scopeStack.Peek();
        parent.Members[newScope.Name] = newScope;
        newScope.Parent = parent;
        scopeStack.Push(newScope);
    }

    public void ExitScope() {
        var current = scopeStack.Peek();
        current.Parent = null;
        scopeStack.Pop();
    }

    public Symbol CurrentScope()
    {
        var current_scope = scopeStack.Peek();
        return current_scope;
    }
    
    public bool Define(Symbol symbol) {
        var currentScope = scopeStack.Peek();
        return this.DefineInScope(currentScope, symbol);
    }

    public bool DefineInScope(Symbol currentScope, Symbol symbol) {
        if (currentScope.Members.ContainsKey(symbol.Name)) {
            return false; // Symbol already defined in the current scope
        }
        symbol.Parent = currentScope;
        currentScope.Members[symbol.Name] = symbol;
        return true;
    }

    public Symbol Resolve(string name) {
        foreach (var scope in scopeStack) {
            if (scope.Members.TryGetValue(name, out var symbol)) {
                return symbol;
            }
        }
        return null; // Symbol not found
    }
}
