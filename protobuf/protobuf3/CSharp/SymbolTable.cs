using System;
using System.Collections.Generic;
using System.Text;

public class SymbolTable {
	private Stack<Symbol> scopeStack = new Stack<Symbol>();

    // Note: predeclared identifiers here.

    public SymbolTable() {
        var globalScope = new Symbol() { Name = "global", Classification = TypeClassification.Package_ };
        scopeStack.Push(globalScope);
    }

    public void EnterScope(Symbol newScope) {
        var current = scopeStack.Peek();
        if (newScope == current) return;
        scopeStack.Push(newScope);
    }

    public void ExitScope() {
        var current = scopeStack.Peek();
        scopeStack.Pop();
        if (scopeStack.Count == 0)
            throw new Exception();
    }

    public Symbol CurrentScope()
    {
        if (scopeStack.Count == 0) return null;
        var current_scope = scopeStack.Peek();
        return current_scope;
    }
    
    public bool Define(Symbol symbol) {
        var currentScope = CurrentScope();
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

    public Symbol Resolve(string name, Symbol start_scope = null)
    {
        if (start_scope == null)
        {
            foreach (Symbol scope in scopeStack)
            {
                if (scope.Members.TryGetValue(name, out Symbol symbol))
                {
                    return symbol;
                }
            }

            return null; // Symbol not found
        }
        else
        {
            if (start_scope.Members.TryGetValue(name, out Symbol symbol))
            {
                return symbol;
            }

            return null; // Symbol not found
        }
    }

    public override string ToString()
    {
        StringBuilder sb = new StringBuilder();
        foreach (var scope in scopeStack)
        {
            foreach (var member in scope.Members)
            {
                sb.AppendLine(member.ToString());
            }
        }
        return sb.ToString();
    }
}
