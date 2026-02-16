package parser

import (
	"fmt"
	"strings"
)

type SymbolTable struct {
	scopeStack   []*Symbol
	blockCounter int
}

func NewSymbolTable() *SymbolTable {
	st := &SymbolTable{}
	globalScope := NewSymbol()
	globalScope.Name = "global"
	globalScope.Classification[Global_] = true
	st.scopeStack = append(st.scopeStack, globalScope)

	// Predefined scalar types
	for _, name := range []string{"integer", "natural", "positive", "float", "long_float",
		"character", "wide_character", "wide_wide_character", "boolean", "duration"} {
		st.defineType(name, false)
	}
	// Predefined composite types
	for _, name := range []string{"string", "wide_string", "wide_wide_string"} {
		st.defineType(name, true)
	}
	// Predefined exceptions
	for _, name := range []string{"constraint_error", "program_error", "storage_error",
		"tasking_error", "numeric_error"} {
		sym := NewSymbol()
		sym.Name = name
		sym.Classification[ExceptionName_] = true
		sym.Predefined = true
		st.Define(sym)
	}
	// Predefined enumeration literals
	for _, name := range []string{"true", "false"} {
		sym := NewSymbol()
		sym.Name = name
		sym.Classification[EnumerationLiteral_] = true
		sym.Predefined = true
		st.Define(sym)
	}
	return st
}

func (st *SymbolTable) defineType(name string, isComposite bool) {
	sym := NewSymbol()
	sym.Name = name
	sym.Classification[TypeName_] = true
	sym.Predefined = true
	sym.IsComposite = isComposite
	st.Define(sym)
}

func (st *SymbolTable) CurrentScope() *Symbol {
	if len(st.scopeStack) == 0 {
		return nil
	}
	return st.scopeStack[len(st.scopeStack)-1]
}

func (st *SymbolTable) Define(symbol *Symbol) bool {
	current := st.CurrentScope()
	if current == nil {
		return false
	}
	symbol.Name = strings.ToLower(symbol.Name)
	if _, exists := current.Members[symbol.Name]; exists {
		return false
	}
	symbol.Parent = current
	current.Members[symbol.Name] = symbol
	return true
}

func (st *SymbolTable) Resolve(name string) *Symbol {
	name = strings.ToLower(name)
	for i := len(st.scopeStack) - 1; i >= 0; i-- {
		if sym, ok := st.scopeStack[i].Members[name]; ok {
			return sym
		}
	}
	return nil
}

func (st *SymbolTable) PushBlockScope() *Symbol {
	st.blockCounter++
	blockScope := NewSymbol()
	blockScope.Name = fmt.Sprintf("block%d", st.blockCounter)
	blockScope.Classification[Block_] = true
	blockScope.Predefined = true
	st.scopeStack = append(st.scopeStack, blockScope)
	return blockScope
}

func (st *SymbolTable) PopBlockScope() {
	if len(st.scopeStack) <= 1 {
		panic("SymbolTable: scope stack underflow")
	}
	st.scopeStack = st.scopeStack[:len(st.scopeStack)-1]
}

func (st *SymbolTable) String() string {
	var sb strings.Builder
	if len(st.scopeStack) > 0 {
		st.toStringHelper(&sb, st.scopeStack[0], 0)
	}
	return sb.String()
}

func (st *SymbolTable) toStringHelper(sb *strings.Builder, scope *Symbol, depth int) {
	indent := strings.Repeat("  ", depth)
	for _, sym := range scope.Members {
		if !sym.Predefined {
			sb.WriteString(indent + sym.String() + "\n")
		}
		if sym.Classification[Block_] || sym.Classification[SubprogramName_] || sym.Classification[PackageName_] {
			st.toStringHelper(sb, sym, depth+1)
		}
	}
}
