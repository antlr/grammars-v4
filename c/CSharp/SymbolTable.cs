using System;
using System.Collections.Generic;
using System.Text;

public class SymbolTable {
    private Stack<Symbol> scopeStack = new Stack<Symbol>();

    public SymbolTable() {
        var globalScope = new Symbol() { Name = "global", Classification = TypeClassification.Global_ };
        scopeStack.Push(globalScope);

		Define(new Symbol() { Name = "typedef", Classification = TypeClassification.StorageClassSpecifier_ });
        Define(new Symbol() { Name = "extern", Classification = TypeClassification.StorageClassSpecifier_ });
        Define(new Symbol() { Name = "static", Classification = TypeClassification.StorageClassSpecifier_ });
        Define(new Symbol() { Name = "_Thread_local", Classification = TypeClassification.StorageClassSpecifier_ });
        Define(new Symbol() { Name = "auto", Classification = TypeClassification.StorageClassSpecifier_ });
        Define(new Symbol() { Name = "register", Classification = TypeClassification.StorageClassSpecifier_ });

        Define(new Symbol() { Name = "struct", Classification = TypeClassification.StorageClassSpecifier_ });
        Define(new Symbol() { Name = "union", Classification = TypeClassification.StorageClassSpecifier_ });

		Define(new Symbol() { Name = "const", Classification = TypeClassification.TypeQualifier_ });
		Define(new Symbol() { Name = "restrict", Classification = TypeClassification.TypeQualifier_ });
		Define(new Symbol() { Name = "volatile", Classification = TypeClassification.TypeQualifier_ });
		Define(new Symbol() { Name = "_Atomic", Classification = TypeClassification.TypeQualifier_ });

        // Init basic types.
        Define(new Symbol() { Name = "void", Classification = TypeClassification.Type_ });
		Define(new Symbol() { Name = "char", Classification = TypeClassification.Type_ });
		Define(new Symbol() { Name = "short", Classification = TypeClassification.Type_ });
		Define(new Symbol() { Name = "int", Classification = TypeClassification.Type_ });
		Define(new Symbol() { Name = "long", Classification = TypeClassification.Type_ });
		Define(new Symbol() { Name = "float", Classification = TypeClassification.Type_ });
		Define(new Symbol() { Name = "double", Classification = TypeClassification.Type_ });
		Define(new Symbol() { Name = "signed", Classification = TypeClassification.Type_ });
		Define(new Symbol() { Name = "unsigned", Classification = TypeClassification.Type_ });
		Define(new Symbol() { Name = "_Bool", Classification = TypeClassification.Type_ });
		Define(new Symbol() { Name = "_Complex", Classification = TypeClassification.Type_ });
		Define(new Symbol() { Name = "__m128", Classification = TypeClassification.Type_ });
		Define(new Symbol() { Name = "__m128d", Classification = TypeClassification.Type_ });
		Define(new Symbol() { Name = "__m128i", Classification = TypeClassification.Type_ });


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
