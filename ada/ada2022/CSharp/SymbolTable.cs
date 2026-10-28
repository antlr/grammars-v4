using System;
using System.Collections.Generic;
using System.Text;

public class SymbolTable {
    private Stack<Symbol> scopeStack = new Stack<Symbol>();
    private int blockCounter = 0;

    public SymbolTable() {
        var globalScope = new Symbol() { Name = "global", Classification = new HashSet<TypeClassification>() {TypeClassification.Global_} };
        scopeStack.Push(globalScope);

        // Predefined scalar types
        DefineType("integer", false);
        DefineType("natural", false);
        DefineType("positive", false);
        DefineType("float", false);
        DefineType("long_float", false);
        DefineType("character", false);
        DefineType("wide_character", false);
        DefineType("wide_wide_character", false);
        DefineType("boolean", false);
        DefineType("duration", false);

        // Predefined composite types
        DefineType("string", true);
        DefineType("wide_string", true);
        DefineType("wide_wide_string", true);

        // Predefined exceptions
        Define(new Symbol() { Name = "constraint_error", Classification = new HashSet<TypeClassification>(){TypeClassification.ExceptionName_}, Predefined = true });
        Define(new Symbol() { Name = "program_error", Classification = new HashSet<TypeClassification>(){TypeClassification.ExceptionName_}, Predefined = true });
        Define(new Symbol() { Name = "storage_error", Classification = new HashSet<TypeClassification>(){TypeClassification.ExceptionName_}, Predefined = true });
        Define(new Symbol() { Name = "tasking_error", Classification = new HashSet<TypeClassification>(){TypeClassification.ExceptionName_}, Predefined = true });
        Define(new Symbol() { Name = "numeric_error", Classification = new HashSet<TypeClassification>(){TypeClassification.ExceptionName_}, Predefined = true });

        // Predefined enumeration literals
        Define(new Symbol() { Name = "true", Classification = new HashSet<TypeClassification>(){TypeClassification.EnumerationLiteral_}, Predefined = true });
        Define(new Symbol() { Name = "false", Classification = new HashSet<TypeClassification>(){TypeClassification.EnumerationLiteral_}, Predefined = true });
    }

    private void DefineType(string name, bool isComposite) {
        Define(new Symbol() { Name = name, Classification = new HashSet<TypeClassification>(){TypeClassification.TypeName_}, Predefined = true, IsComposite = isComposite });
    }

    public void EnterScope(Symbol newScope) {
        var current = scopeStack.Peek();
        if (newScope == current) return;
        scopeStack.Push(newScope);
    }

    public void ExitScope() {
        scopeStack.Pop();
        if (scopeStack.Count == 0)
            throw new Exception("SymbolTable: scope stack underflow");
    }

    public Symbol CurrentScope()
    {
        if (scopeStack.Count == 0) return null;
        return scopeStack.Peek();
    }

    public bool Define(Symbol symbol) {
        var currentScope = CurrentScope();
        return this.DefineInScope(currentScope, symbol);
    }

    public bool DefineInScope(Symbol currentScope, Symbol symbol) {
        // Case-insensitive: normalize name to lowercase
        symbol.Name = symbol.Name.ToLowerInvariant();
        if (currentScope.Members.ContainsKey(symbol.Name)) {
            return false;
        }
        symbol.Parent = currentScope;
        currentScope.Members[symbol.Name] = symbol;
        return true;
    }

    public Symbol Resolve(string name, Symbol startScope = null)
    {
        // Case-insensitive: normalize to lowercase
        name = name.ToLowerInvariant();
        if (startScope == null)
        {
            foreach (Symbol scope in scopeStack)
            {
                if (scope.Members.TryGetValue(name, out Symbol symbol))
                {
                    return symbol;
                }
            }
            return null;
        }
        else
        {
            if (startScope.Members.TryGetValue(name, out Symbol symbol))
            {
                return symbol;
            }
            return null;
        }
    }

    public Symbol PushBlockScope()
    {
        var blockScope = new Symbol()
        {
            Name = "block" + (++blockCounter),
            Classification = new HashSet<TypeClassification>() { TypeClassification.Block_ },
            Predefined = true
        };
        EnterScope(blockScope);
        return blockScope;
    }

    public void PopBlockScope()
    {
        ExitScope();
    }

    public List<Symbol> GetExportedSymbols()
    {
        var result = new List<Symbol>();
        var global = scopeStack.ToArray()[scopeStack.Count - 1]; // bottom of stack = global scope
        foreach (var entry in global.Members)
        {
            if (!entry.Value.Predefined)
                result.Add(entry.Value);
        }
        return result;
    }

    public override string ToString()
    {
        StringBuilder sb = new StringBuilder();
        var scopes = scopeStack.ToArray();
        if (scopes.Length > 0)
        {
            ToStringHelper(sb, scopes[scopes.Length - 1], 0);
        }
        return sb.ToString();
    }

    private void ToStringHelper(StringBuilder sb, Symbol scope, int depth)
    {
        string indent = new string(' ', depth * 2);
        foreach (var entry in scope.Members)
        {
            var sym = entry.Value;
            if (!sym.Predefined)
            {
                sb.AppendLine(indent + sym.ToString());
            }
            if (sym.Classification.Contains(TypeClassification.Block_) ||
                sym.Classification.Contains(TypeClassification.SubprogramName_) ||
                sym.Classification.Contains(TypeClassification.PackageName_))
            {
                ToStringHelper(sb, sym, depth + 1);
            }
        }
    }
}
