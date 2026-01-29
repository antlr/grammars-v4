using System;
using System.Collections.Generic;
using System.Text;

public class SymbolTable {
    private Stack<Symbol> scopeStack = new Stack<Symbol>();
    private int blockCounter = 0;

    public SymbolTable() {
        var globalScope = new Symbol() { Name = "global", Classification = new HashSet<TypeClassification>() {TypeClassification.Global_} };
        scopeStack.Push(globalScope);

        Define(new Symbol() { Name = "auto", Classification = new HashSet<TypeClassification>(){TypeClassification.StorageClassSpecifier_}, Predefined = true });
        Define(new Symbol() { Name = "constexpr", Classification = new HashSet<TypeClassification>() { TypeClassification.StorageClassSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "extern", Classification = new HashSet<TypeClassification>() { TypeClassification.StorageClassSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "register", Classification = new HashSet<TypeClassification>() { TypeClassification.StorageClassSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "static", Classification = new HashSet<TypeClassification>() { TypeClassification.StorageClassSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "thread_local", Classification = new HashSet<TypeClassification>() { TypeClassification.StorageClassSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "_Thread_local", Classification = new HashSet<TypeClassification>() { TypeClassification.StorageClassSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "typedef", Classification = new HashSet<TypeClassification>() { TypeClassification.StorageClassSpecifier_ }, Predefined = true });

        Define(new Symbol() { Name = "enum", Classification = new HashSet<TypeClassification>() { TypeClassification.EnumSpecifier_ }, Predefined = true });

        Define(new Symbol() { Name = "struct", Classification = new HashSet<TypeClassification>() { TypeClassification.StorageClassSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "union", Classification = new HashSet<TypeClassification>() { TypeClassification.StorageClassSpecifier_ }, Predefined = true });

        Define(new Symbol() { Name = "const", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeQualifier_ }, Predefined = true });
        Define(new Symbol() { Name = "restrict", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeQualifier_ }, Predefined = true });
        Define(new Symbol() { Name = "volatile", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeQualifier_ }, Predefined = true });
        Define(new Symbol() { Name = "_Atomic", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeQualifier_, TypeClassification.AtomicTypeSpecifier_}, Predefined = true });

        Define(new Symbol() { Name = "void", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "char", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "short", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "int", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "long", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "float", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "double", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "signed", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "unsigned", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "_BitInt", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "bool", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "_Bool", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "_Complex", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "_Decimal32", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "_Decimal64", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "_Decimal128", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__m128", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__m128d", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__m128i", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__extension__", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });

        Define(new Symbol() { Name = "__builtin_va_list", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_has_attribute", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_speculation_safe_value", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_types_compatible_p", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_choose_expr", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_tgmath", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_constant_p", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_is_constant_evaluated", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_bit_cast", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_expect", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_expect_with_probability", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_trap", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
//        Define(new Symbol() { Name = "__builtin_unreachable", Classification = new HashSet<TypeClassification>() { TypeClassification.FunctionSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_assoc_barrier", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_assume_aligned", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_LINE", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_FUNCTION", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_FILE", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin___clear_cache", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_prefetch", Classification = new HashSet<TypeClassification>() { TypeClassification.Function_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_classify_type", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_extend_pointer", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_goacc_parlevel_id", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__builtin_goacc_parlevel_size", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true });

        Define(new Symbol() { Name = "inline", Classification = new HashSet<TypeClassification>() { TypeClassification.FunctionSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "_Noreturn", Classification = new HashSet<TypeClassification>() { TypeClassification.FunctionSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "__inline__", Classification = new HashSet<TypeClassification>() { TypeClassification.FunctionSpecifier_ }, Predefined = true });
        
        Define(new Symbol() { Name = "__cdecl", Classification = new HashSet<TypeClassification>() { TypeClassification.FunctionSpecifier_ }, Predefined = true }); // MS
        Define(new Symbol() { Name = "__clrcall", Classification = new HashSet<TypeClassification>() { TypeClassification.FunctionSpecifier_ }, Predefined = true }); // MS
        Define(new Symbol() { Name = "__stdcall", Classification = new HashSet<TypeClassification>() { TypeClassification.FunctionSpecifier_ }, Predefined = true }); // MS
        Define(new Symbol() { Name = "__fastcall", Classification = new HashSet<TypeClassification>() { TypeClassification.FunctionSpecifier_ }, Predefined = true }); // MS
        Define(new Symbol() { Name = "__thiscall", Classification = new HashSet<TypeClassification>() { TypeClassification.FunctionSpecifier_ }, Predefined = true }); // MS
        Define(new Symbol() { Name = "__vectorcall", Classification = new HashSet<TypeClassification>() { TypeClassification.FunctionSpecifier_ }, Predefined = true }); // MS

        Define(new Symbol() { Name = "_purecall", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        Define(new Symbol() { Name = "_purecall_handler", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        Define(new Symbol() { Name = "_onexit_t", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        Define(new Symbol() { Name = "_locale_t", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        Define(new Symbol() { Name = "_invalid_parameter_handler", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        Define(new Symbol() { Name = "__inline", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        

        Define(new Symbol() { Name = "__int8", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        Define(new Symbol() { Name = "__int16", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        Define(new Symbol() { Name = "__int32", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        Define(new Symbol() { Name = "__int64", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        Define(new Symbol() { Name = "__int128", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        Define(new Symbol() { Name = "_Float16", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        Define(new Symbol() { Name = "_Float32", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        Define(new Symbol() { Name = "_Float64", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        Define(new Symbol() { Name = "_Float128", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        Define(new Symbol() { Name = "__v8hf", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        Define(new Symbol() { Name = "__bf16", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        Define(new Symbol() { Name = "__v16bf", Classification = new HashSet<TypeClassification>() { TypeClassification.TypeSpecifier_ }, Predefined = true }); // gcc
        
        Define(new Symbol() { Name = "__declspec", Classification = new HashSet<TypeClassification>() { TypeClassification.FunctionSpecifier_ }, Predefined = true }); // MS
        Define(new Symbol() { Name = "__attribute__", Classification = new HashSet<TypeClassification>() { TypeClassification.FunctionSpecifier_ }, Predefined = true }); // GCC

        Define(new Symbol() { Name = "alignas", Classification = new HashSet<TypeClassification>() { TypeClassification.AlignmentSpecifier_ }, Predefined = true });
        Define(new Symbol() { Name = "align", Classification = new HashSet<TypeClassification>() { TypeClassification.AlignmentSpecifier_ }, Predefined = true });
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
            // Stack iteration in C# is already from top (innermost) to bottom (outermost)
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

    public override string ToString()
    {
        StringBuilder sb = new StringBuilder();
        // Get the global scope (bottom of stack)
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
            // Recursively print nested scopes
            if (sym.Classification.Contains(TypeClassification.Block_) ||
                sym.Classification.Contains(TypeClassification.Function_))
            {
                ToStringHelper(sb, sym, depth + 1);
            }
        }
    }
}
