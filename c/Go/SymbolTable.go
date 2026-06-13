package parser

import "strings"

type SymbolTable struct {
	scopeStack   []*Symbol
	blockCounter int
}

func NewSymbolTable() *SymbolTable {
	st := &SymbolTable{}
	global := newSymbol("global", Global_)
	st.scopeStack = append(st.scopeStack, global)

	st.define(newPredefinedSymbol("auto", StorageClassSpecifier_))
	st.define(newPredefinedSymbol("constexpr", StorageClassSpecifier_))
	st.define(newPredefinedSymbol("extern", StorageClassSpecifier_))
	st.define(newPredefinedSymbol("register", StorageClassSpecifier_))
	st.define(newPredefinedSymbol("static", StorageClassSpecifier_))
	st.define(newPredefinedSymbol("thread_local", StorageClassSpecifier_))
	st.define(newPredefinedSymbol("_Thread_local", StorageClassSpecifier_))
	st.define(newPredefinedSymbol("typedef", StorageClassSpecifier_))

	st.define(newPredefinedSymbol("enum", EnumSpecifier_))

	st.define(newPredefinedSymbol("struct", StorageClassSpecifier_))
	st.define(newPredefinedSymbol("union", StorageClassSpecifier_))

	st.define(newPredefinedSymbol("const", TypeQualifier_))
	st.define(newPredefinedSymbol("restrict", TypeQualifier_))
	st.define(newPredefinedSymbol("volatile", TypeQualifier_))
	atomic := newPredefinedSymbol("_Atomic", TypeQualifier_, AtomicTypeSpecifier_)
	st.define(atomic)

	st.define(newPredefinedSymbol("void", TypeSpecifier_))
	st.define(newPredefinedSymbol("char", TypeSpecifier_))
	st.define(newPredefinedSymbol("short", TypeSpecifier_))
	st.define(newPredefinedSymbol("int", TypeSpecifier_))
	st.define(newPredefinedSymbol("long", TypeSpecifier_))
	st.define(newPredefinedSymbol("float", TypeSpecifier_))
	st.define(newPredefinedSymbol("double", TypeSpecifier_))
	st.define(newPredefinedSymbol("signed", TypeSpecifier_))
	st.define(newPredefinedSymbol("unsigned", TypeSpecifier_))
	st.define(newPredefinedSymbol("_BitInt", TypeSpecifier_))
	st.define(newPredefinedSymbol("bool", TypeSpecifier_))
	st.define(newPredefinedSymbol("_Bool", TypeSpecifier_))
	st.define(newPredefinedSymbol("_Complex", TypeSpecifier_))
	st.define(newPredefinedSymbol("_Decimal32", TypeSpecifier_))
	st.define(newPredefinedSymbol("_Decimal64", TypeSpecifier_))
	st.define(newPredefinedSymbol("_Decimal128", TypeSpecifier_))
	st.define(newPredefinedSymbol("__m128", TypeSpecifier_))
	st.define(newPredefinedSymbol("__m128d", TypeSpecifier_))
	st.define(newPredefinedSymbol("__m128i", TypeSpecifier_))
	st.define(newPredefinedSymbol("__extension__", TypeSpecifier_))

	st.define(newPredefinedSymbol("__builtin_va_list", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_has_attribute", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_speculation_safe_value", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_types_compatible_p", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_choose_expr", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_tgmath", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_constant_p", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_is_constant_evaluated", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_bit_cast", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_expect", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_expect_with_probability", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_trap", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_assoc_barrier", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_assume_aligned", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_LINE", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_FUNCTION", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_FILE", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin___clear_cache", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_prefetch", Function_))
	st.define(newPredefinedSymbol("__builtin_classify_type", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_extend_pointer", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_goacc_parlevel_id", TypeSpecifier_))
	st.define(newPredefinedSymbol("__builtin_goacc_parlevel_size", TypeSpecifier_))

	st.define(newPredefinedSymbol("inline", FunctionSpecifier_))
	st.define(newPredefinedSymbol("_Noreturn", FunctionSpecifier_))
	st.define(newPredefinedSymbol("__inline__", FunctionSpecifier_))

	st.define(newPredefinedSymbol("__cdecl", FunctionSpecifier_))
	st.define(newPredefinedSymbol("__clrcall", FunctionSpecifier_))
	st.define(newPredefinedSymbol("__stdcall", FunctionSpecifier_))
	st.define(newPredefinedSymbol("__fastcall", FunctionSpecifier_))
	st.define(newPredefinedSymbol("__thiscall", FunctionSpecifier_))
	st.define(newPredefinedSymbol("__vectorcall", FunctionSpecifier_))

	st.define(newPredefinedSymbol("_purecall", TypeSpecifier_))
	st.define(newPredefinedSymbol("_purecall_handler", TypeSpecifier_))
	st.define(newPredefinedSymbol("_onexit_t", TypeSpecifier_))
	st.define(newPredefinedSymbol("_locale_t", TypeSpecifier_))
	st.define(newPredefinedSymbol("_invalid_parameter_handler", TypeSpecifier_))
	st.define(newPredefinedSymbol("__inline", TypeSpecifier_))

	st.define(newPredefinedSymbol("__int8", TypeSpecifier_))
	st.define(newPredefinedSymbol("__int16", TypeSpecifier_))
	st.define(newPredefinedSymbol("__int32", TypeSpecifier_))
	st.define(newPredefinedSymbol("__int64", TypeSpecifier_))
	st.define(newPredefinedSymbol("__int128", TypeSpecifier_))
	st.define(newPredefinedSymbol("_Float16", TypeSpecifier_))
	st.define(newPredefinedSymbol("_Float32", TypeSpecifier_))
	st.define(newPredefinedSymbol("_Float64", TypeSpecifier_))
	st.define(newPredefinedSymbol("_Float128", TypeSpecifier_))
	st.define(newPredefinedSymbol("__v8hf", TypeSpecifier_))
	st.define(newPredefinedSymbol("__bf16", TypeSpecifier_))
	st.define(newPredefinedSymbol("__v16bf", TypeSpecifier_))

	st.define(newPredefinedSymbol("__declspec", FunctionSpecifier_))
	st.define(newPredefinedSymbol("__attribute__", FunctionSpecifier_))

	st.define(newPredefinedSymbol("alignas", AlignmentSpecifier_))
	st.define(newPredefinedSymbol("align", AlignmentSpecifier_))

	return st
}

func (st *SymbolTable) currentScope() *Symbol {
	if len(st.scopeStack) == 0 {
		return nil
	}
	return st.scopeStack[len(st.scopeStack)-1]
}

func (st *SymbolTable) enterScope(newScope *Symbol) {
	current := st.currentScope()
	if newScope == current {
		return
	}
	st.scopeStack = append(st.scopeStack, newScope)
}

func (st *SymbolTable) exitScope() {
	if len(st.scopeStack) <= 1 {
		panic("cannot pop global scope")
	}
	st.scopeStack = st.scopeStack[:len(st.scopeStack)-1]
}

func (st *SymbolTable) define(symbol *Symbol) bool {
	current := st.currentScope()
	return st.defineInScope(current, symbol)
}

func (st *SymbolTable) defineInScope(scope *Symbol, symbol *Symbol) bool {
	if _, exists := scope.Members[symbol.Name]; exists {
		return false
	}
	symbol.Parent = scope
	scope.Members[symbol.Name] = symbol
	return true
}

// Define adds a symbol to the current scope.
func (st *SymbolTable) Define(symbol *Symbol) bool {
	return st.define(symbol)
}

// Resolve looks up a symbol by name, walking outward from the innermost scope.
// If startScope is non-nil, only searches that scope.
func (st *SymbolTable) Resolve(name string, startScope *Symbol) *Symbol {
	if startScope != nil {
		if sym, ok := startScope.Members[name]; ok {
			return sym
		}
		return nil
	}
	for i := len(st.scopeStack) - 1; i >= 0; i-- {
		if sym, ok := st.scopeStack[i].Members[name]; ok {
			return sym
		}
	}
	return nil
}

// PushBlockScope pushes a new anonymous block scope.
func (st *SymbolTable) PushBlockScope() *Symbol {
	st.blockCounter++
	block := newPredefinedSymbol("block"+itoa(st.blockCounter), Block_)
	st.enterScope(block)
	return block
}

// PopBlockScope pops the current block scope.
func (st *SymbolTable) PopBlockScope() {
	st.exitScope()
}

func (st *SymbolTable) String() string {
	var sb strings.Builder
	if len(st.scopeStack) > 0 {
		toStringHelper(&sb, st.scopeStack[0], 0)
	}
	return sb.String()
}

func toStringHelper(sb *strings.Builder, scope *Symbol, depth int) {
	indent := strings.Repeat("  ", depth)
	for _, sym := range scope.Members {
		if !sym.Predefined {
			sb.WriteString(indent + sym.String() + "\n")
		}
		if sym.Classification[Block_] || sym.Classification[Function_] {
			toStringHelper(sb, sym, depth+1)
		}
	}
}

func itoa(i int) string {
	if i == 0 {
		return "0"
	}
	neg := false
	if i < 0 {
		neg = true
		i = -i
	}
	buf := make([]byte, 0, 10)
	for i > 0 {
		buf = append([]byte{byte('0' + i%10)}, buf...)
		i /= 10
	}
	if neg {
		buf = append([]byte{'-'}, buf...)
	}
	return string(buf)
}
