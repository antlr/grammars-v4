import java.util.HashSet;
import java.util.Stack;

public class SymbolTable {
    private Stack<Symbol> scopeStack = new Stack<>();
    private int blockCounter = 0;

    public SymbolTable() {
        Symbol globalScope = createSymbol("global", TypeClassification.Global_);
        scopeStack.push(globalScope);

        define(createSymbol("auto", TypeClassification.StorageClassSpecifier_));
        define(createSymbol("constexpr", TypeClassification.StorageClassSpecifier_));
        define(createSymbol("extern", TypeClassification.StorageClassSpecifier_));
        define(createSymbol("register", TypeClassification.StorageClassSpecifier_));
        define(createSymbol("static", TypeClassification.StorageClassSpecifier_));
        define(createSymbol("thread_local", TypeClassification.StorageClassSpecifier_));
        define(createSymbol("_Thread_local", TypeClassification.StorageClassSpecifier_));
        define(createSymbol("typedef", TypeClassification.StorageClassSpecifier_));

        define(createSymbol("enum", TypeClassification.EnumSpecifier_));

        define(createSymbol("struct", TypeClassification.StorageClassSpecifier_));
        define(createSymbol("union", TypeClassification.StorageClassSpecifier_));

        define(createSymbol("const", TypeClassification.TypeQualifier_));
        define(createSymbol("restrict", TypeClassification.TypeQualifier_));
        define(createSymbol("volatile", TypeClassification.TypeQualifier_));
        define(createSymbol("_Atomic", TypeClassification.TypeQualifier_, TypeClassification.AtomicTypeSpecifier_));

        define(createSymbol("void", TypeClassification.TypeSpecifier_));
        define(createSymbol("char", TypeClassification.TypeSpecifier_));
        define(createSymbol("short", TypeClassification.TypeSpecifier_));
        define(createSymbol("int", TypeClassification.TypeSpecifier_));
        define(createSymbol("long", TypeClassification.TypeSpecifier_));
        define(createSymbol("float", TypeClassification.TypeSpecifier_));
        define(createSymbol("double", TypeClassification.TypeSpecifier_));
        define(createSymbol("signed", TypeClassification.TypeSpecifier_));
        define(createSymbol("unsigned", TypeClassification.TypeSpecifier_));
        define(createSymbol("_BitInt", TypeClassification.TypeSpecifier_));
        define(createSymbol("bool", TypeClassification.TypeSpecifier_));
        define(createSymbol("_Bool", TypeClassification.TypeSpecifier_));
        define(createSymbol("_Complex", TypeClassification.TypeSpecifier_));
        define(createSymbol("_Decimal32", TypeClassification.TypeSpecifier_));
        define(createSymbol("_Decimal64", TypeClassification.TypeSpecifier_));
        define(createSymbol("_Decimal128", TypeClassification.TypeSpecifier_));
        define(createSymbol("__m128", TypeClassification.TypeSpecifier_));
        define(createSymbol("__m128d", TypeClassification.TypeSpecifier_));
        define(createSymbol("__m128i", TypeClassification.TypeSpecifier_));
        define(createSymbol("__extension__", TypeClassification.TypeSpecifier_));

        define(createSymbol("__builtin_va_list", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_has_attribute", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_speculation_safe_value", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_types_compatible_p", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_choose_expr", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_tgmath", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_constant_p", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_is_constant_evaluated", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_bit_cast", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_expect", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_expect_with_probability", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_trap", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_assoc_barrier", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_assume_aligned", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_LINE", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_FUNCTION", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_FILE", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin___clear_cache", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_prefetch", TypeClassification.Function_));
        define(createSymbol("__builtin_classify_type", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_extend_pointer", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_goacc_parlevel_id", TypeClassification.TypeSpecifier_));
        define(createSymbol("__builtin_goacc_parlevel_size", TypeClassification.TypeSpecifier_));

        define(createSymbol("inline", TypeClassification.FunctionSpecifier_));
        define(createSymbol("_Noreturn", TypeClassification.FunctionSpecifier_));
        define(createSymbol("__inline__", TypeClassification.FunctionSpecifier_));

        define(createSymbol("__cdecl", TypeClassification.FunctionSpecifier_)); // MS
        define(createSymbol("__clrcall", TypeClassification.FunctionSpecifier_)); // MS
        define(createSymbol("__stdcall", TypeClassification.FunctionSpecifier_)); // MS
        define(createSymbol("__fastcall", TypeClassification.FunctionSpecifier_)); // MS
        define(createSymbol("__thiscall", TypeClassification.FunctionSpecifier_)); // MS
        define(createSymbol("__vectorcall", TypeClassification.FunctionSpecifier_)); // MS

        define(createSymbol("_purecall", TypeClassification.TypeSpecifier_)); // gcc
        define(createSymbol("_purecall_handler", TypeClassification.TypeSpecifier_)); // gcc
        define(createSymbol("_onexit_t", TypeClassification.TypeSpecifier_)); // gcc
        define(createSymbol("_locale_t", TypeClassification.TypeSpecifier_)); // gcc
        define(createSymbol("_invalid_parameter_handler", TypeClassification.TypeSpecifier_)); // gcc
        define(createSymbol("__inline", TypeClassification.TypeSpecifier_)); // gcc

        define(createSymbol("__int8", TypeClassification.TypeSpecifier_)); // gcc
        define(createSymbol("__int16", TypeClassification.TypeSpecifier_)); // gcc
        define(createSymbol("__int32", TypeClassification.TypeSpecifier_)); // gcc
        define(createSymbol("__int64", TypeClassification.TypeSpecifier_)); // gcc
        define(createSymbol("__int128", TypeClassification.TypeSpecifier_)); // gcc
        define(createSymbol("_Float16", TypeClassification.TypeSpecifier_)); // gcc
        define(createSymbol("_Float32", TypeClassification.TypeSpecifier_)); // gcc
        define(createSymbol("_Float64", TypeClassification.TypeSpecifier_)); // gcc
        define(createSymbol("_Float128", TypeClassification.TypeSpecifier_)); // gcc
        define(createSymbol("__v8hf", TypeClassification.TypeSpecifier_)); // gcc
        define(createSymbol("__bf16", TypeClassification.TypeSpecifier_)); // gcc
        define(createSymbol("__v16bf", TypeClassification.TypeSpecifier_)); // gcc

        define(createSymbol("__declspec", TypeClassification.FunctionSpecifier_)); // MS
        define(createSymbol("__attribute__", TypeClassification.FunctionSpecifier_)); // GCC

        define(createSymbol("alignas", TypeClassification.AlignmentSpecifier_));
        define(createSymbol("align", TypeClassification.AlignmentSpecifier_));
    }

    private Symbol createSymbol(String name, TypeClassification... classifications) {
        Symbol symbol = new Symbol();
        symbol.setName(name);
        HashSet<TypeClassification> classSet = new HashSet<>();
        for (TypeClassification c : classifications) {
            classSet.add(c);
        }
        symbol.setClassification(classSet);
        symbol.setPredefined(true);
        return symbol;
    }

    public void enterScope(Symbol newScope) {
        Symbol current = scopeStack.peek();
        if (newScope == current) return;
        scopeStack.push(newScope);
    }

    public void exitScope() {
        scopeStack.pop();
        if (scopeStack.isEmpty()) {
            throw new RuntimeException("Cannot exit global scope");
        }
    }

    public Symbol currentScope() {
        if (scopeStack.isEmpty()) return null;
        return scopeStack.peek();
    }

    public boolean define(Symbol symbol) {
        Symbol currentScope = currentScope();
        return defineInScope(currentScope, symbol);
    }

    public boolean defineInScope(Symbol currentScope, Symbol symbol) {
        if (currentScope.getMembers().containsKey(symbol.getName())) {
            return false; // Symbol already defined in the current scope
        }
        symbol.setParent(currentScope);
        currentScope.getMembers().put(symbol.getName(), symbol);
        return true;
    }

    public Symbol resolve(String name) {
        return resolve(name, null);
    }

    public Symbol resolve(String name, Symbol startScope) {
        if (startScope == null) {
            // Iterate from innermost (top of stack) to outermost (bottom of stack)
            for (int i = scopeStack.size() - 1; i >= 0; i--) {
                Symbol scope = scopeStack.get(i);
                Symbol symbol = scope.getMembers().get(name);
                if (symbol != null) {
                    return symbol;
                }
            }
            return null; // Symbol not found
        } else {
            return startScope.getMembers().get(name);
        }
    }

    public Symbol pushBlockScope() {
        Symbol blockScope = new Symbol();
        blockScope.setName("block" + (++blockCounter));
        HashSet<TypeClassification> classSet = new HashSet<>();
        classSet.add(TypeClassification.Block_);
        blockScope.setClassification(classSet);
        blockScope.setPredefined(true);
        enterScope(blockScope);
        return blockScope;
    }

    public void popBlockScope() {
        exitScope();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        toStringHelper(sb, scopeStack.get(0), 0);
        return sb.toString();
    }

    private void toStringHelper(StringBuilder sb, Symbol scope, int depth) {
        String indent = "  ".repeat(depth);
        for (var entry : scope.getMembers().entrySet()) {
            Symbol sym = entry.getValue();
            if (!sym.isPredefined()) {
                sb.append(indent).append(sym.toString()).append("\n");
            }
            // Recursively print nested scopes
            if (sym.getClassification().contains(TypeClassification.Block_) ||
                sym.getClassification().contains(TypeClassification.Function_)) {
                toStringHelper(sb, sym, depth + 1);
            }
        }
    }
}
