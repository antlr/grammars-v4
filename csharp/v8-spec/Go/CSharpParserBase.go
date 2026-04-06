package parser

import (
	"fmt"
	"os"
	"strings"
	"unicode"

	"github.com/antlr4-go/antlr/v4"
)

type CSharpParserBase struct {
	*antlr.BaseParser
	symTable       *CSharpSymbolTable
	pendingVarType string
}

// -------------------------------------------------------------------------
// Look-ahead helpers
// -------------------------------------------------------------------------

func (p *CSharpParserBase) LookAheadIs(pos int, value int) bool {
	return p.GetTokenStream().LA(pos) == value
}

func (p *CSharpParserBase) LookAheadIsNot(pos int, value int) bool {
	return p.GetTokenStream().LA(pos) != value
}

// -------------------------------------------------------------------------
// Error reporting
// -------------------------------------------------------------------------

func (p *CSharpParserBase) notifySemanticError(line int, charPositionInLine int, msg string) {
	p.NotifyErrorListeners(msg, nil, nil)
	fmt.Fprintf(os.Stderr, "%d:%d %s\n", line, charPositionInLine, msg)
}

// -------------------------------------------------------------------------
// ReduceTree — post-parse cleanup (for external tooling)
// -------------------------------------------------------------------------

func (p *CSharpParserBase) ReduceTree(currentctx antlr.ParserRuleContext) {
	ruleNames := p.GetRuleNames()
	ruleTypeArgumentList := -1
	for i, name := range ruleNames {
		if name == "type_argument_list" {
			ruleTypeArgumentList = i
			break
		}
	}

	var takeOutEmpties func(node antlr.ParserRuleContext)
	takeOutEmpties = func(node antlr.ParserRuleContext) {
		if node == nil {
			return
		}
		base, ok := node.(*antlr.BaseParserRuleContext)
		if !ok || base == nil {
			return
		}
		for i := node.GetChildCount() - 1; i >= 0; i-- {
			child := node.GetChild(i)
			childRule, ok := child.(antlr.ParserRuleContext)
			if !ok {
				continue
			}
			if childRule.GetRuleIndex() == ruleTypeArgumentList && childRule.GetChildCount() == 0 {
				base.RemoveLastChild()
			} else {
				takeOutEmpties(childRule)
			}
		}
	}
	takeOutEmpties(currentctx)
}

// -------------------------------------------------------------------------
// insertNode — rewire parse tree (MLR un-inlining)
// NOTE: The specific context constructors (Invocation_expressionContext, etc.)
// are not generated for all grammar targets. This implementation is a no-op
// because the required New*Context functions do not exist in the generated
// CSharpParser.go for this target.
// -------------------------------------------------------------------------

func (p *CSharpParserBase) insertNode(currentctx antlr.ParserRuleContext, contextTypeName string) {
	// no-op: context constructors not available in generated Go parser
	_ = currentctx
	_ = contextTypeName
}

func (p *CSharpParserBase) AsInvocationExpression(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Invocation_expressionContext")
}

func (p *CSharpParserBase) AsElementAccess(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Element_accessContext")
}

func (p *CSharpParserBase) AsMemberAccess(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Member_accessContext")
}

func (p *CSharpParserBase) AsNullConditionalMemberAccess(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Null_conditional_member_accessContext")
}

func (p *CSharpParserBase) AsNullConditionalElementAccess(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Null_conditional_element_accessContext")
}

func (p *CSharpParserBase) AsPostIncrementExpression(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Post_increment_expressionContext")
}

func (p *CSharpParserBase) AsPostDecrementExpression(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Post_decrement_expressionContext")
}

func (p *CSharpParserBase) AsNullForgivingExpression(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Null_forgiving_expressionContext")
}

func (p *CSharpParserBase) AsPointerMemberAccess(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Pointer_member_accessContext")
}

// -------------------------------------------------------------------------
// ElementAccessSemanticCheck
// -------------------------------------------------------------------------

func (p *CSharpParserBase) ElementAccessSemanticCheck(currentctx antlr.ParserRuleContext) {
	ruleNames := p.GetRuleNames()

	ruleIdx := func(name string) int {
		for i, n := range ruleNames {
			if n == name {
				return i
			}
		}
		return -1
	}

	rulePrimaryExpression            := ruleIdx("primary_expression")
	ruleElementAccess                := ruleIdx("element_access")
	rulePointerElementAccess         := ruleIdx("pointer_element_access")
	ruleNullConditionalElementAccess := ruleIdx("null_conditional_element_access")
	ruleArrayCreationExpression      := ruleIdx("array_creation_expression")
	ruleStackallocExpression         := ruleIdx("stackalloc_expression")

	if currentctx == nil || currentctx.GetRuleIndex() != rulePrimaryExpression || currentctx.GetChildCount() != 1 {
		return
	}

	childTree := currentctx.GetChild(0)
	childRule, ok := childTree.(antlr.ParserRuleContext)
	if !ok {
		return
	}

	childRuleIndex  := childRule.GetRuleIndex()
	childChildCount := childRule.GetChildCount()

	if childRuleIndex == ruleElementAccess || childRuleIndex == rulePointerElementAccess {
		if childChildCount != 4 {
			return
		}
	} else if childRuleIndex == ruleNullConditionalElementAccess {
		if childChildCount != 5 {
			return
		}
	} else {
		return
	}

	accessTargetTree := childRule.GetChild(0)
	accessTarget, ok := accessTargetTree.(antlr.ParserRuleContext)
	if !ok {
		return
	}
	if accessTarget.GetRuleIndex() != rulePrimaryExpression || accessTarget.GetChildCount() == 0 {
		return
	}

	lhsTargetTree := accessTarget.GetChild(0)
	lhsTarget, ok := lhsTargetTree.(antlr.ParserRuleContext)
	if !ok {
		return
	}

	if lhsTarget.GetRuleIndex() != ruleArrayCreationExpression &&
		lhsTarget.GetRuleIndex() != ruleStackallocExpression {
		return
	}

	lhsLast := lhsTarget.GetStop()
	if lhsLast == nil {
		return
	}
	lhsLastType := lhsLast.GetTokenType()

	if lhsLastType == CSharpLexerTK_RBRACE {
		return // initializer present
	}

	if lhsLastType != CSharpLexerTK_RBRACK {
		fmt.Fprintf(os.Stderr, "%d:%d Error: Unexpected LHS last token %s (%d).\n",
			lhsLast.GetLine(), lhsLast.GetColumn(), lhsLast.GetText(), lhsLastType)
		return
	}

	childRuleName := ruleNames[childRuleIndex]
	lhsRuleName   := ruleNames[lhsTarget.GetRuleIndex()]
	childPrefix := "a"
	if len(childRuleName) > 0 && strings.ContainsRune("AEIOUaeiou", rune(childRuleName[0])) {
		childPrefix = "an"
	}
	lhsPrefix := "a"
	if len(lhsRuleName) > 0 && strings.ContainsRune("AEIOUaeiou", rune(lhsRuleName[0])) {
		lhsPrefix = "an"
	}

	p.notifySemanticError(lhsLast.GetLine(), lhsLast.GetColumn(),
		fmt.Sprintf("LHS of %s %s cannot be %s %s unless it has an initializer",
			childPrefix, childRuleName, lhsPrefix, lhsRuleName))
}

// =============================================================================
// Symbol table
// =============================================================================

// CSharpSymbolKind enumerates what a symbol represents.
type CSharpSymbolKind int

const (
	CSharpSymbolKindType          CSharpSymbolKind = iota
	CSharpSymbolKindVariable
	CSharpSymbolKindNamespace
	CSharpSymbolKindTypeParameter
	CSharpSymbolKindAlias
)

// CSharpTypeKind enumerates type categories.
type CSharpTypeKind int

const (
	CSharpTypeKindClass     CSharpTypeKind = iota
	CSharpTypeKindStruct
	CSharpTypeKindInterface
	CSharpTypeKindEnum
	CSharpTypeKindDelegate
)

// CSharpScopeKind enumerates scope categories.
type CSharpScopeKind int

const (
	CSharpScopeKindGlobal    CSharpScopeKind = iota
	CSharpScopeKindNamespace
	CSharpScopeKindType
	CSharpScopeKindMethod
	CSharpScopeKindBlock
)

// CSharpSymbol is a declared symbol.
type CSharpSymbol struct {
	Name     string
	Kind     CSharpSymbolKind
	TypeKind CSharpTypeKind
	TypeRef  string
	Target   string
}

// CSharpScope is one level in the scope chain.
type CSharpScope struct {
	Kind    CSharpScopeKind
	Parent  *CSharpScope
	Name    string
	symbols map[string]*CSharpSymbol
}

func newCSharpScope(kind CSharpScopeKind, parent *CSharpScope, name string) *CSharpScope {
	return &CSharpScope{Kind: kind, Parent: parent, Name: name, symbols: make(map[string]*CSharpSymbol)}
}

func (s *CSharpScope) Declare(sym *CSharpSymbol)        { s.symbols[sym.Name] = sym }
func (s *CSharpScope) Lookup(name string) *CSharpSymbol { return s.symbols[name] }
func (s *CSharpScope) LookupChain(name string) *CSharpSymbol {
	if sym := s.Lookup(name); sym != nil {
		return sym
	}
	if s.Parent != nil {
		return s.Parent.LookupChain(name)
	}
	return nil
}

// CSharpSymbolTable is the parse-time symbol table.
type CSharpSymbolTable struct {
	knownTypeNames map[string]bool
	genericArities map[string]map[int]bool
	scopeStack     []*CSharpScope
	GlobalScope    *CSharpScope
}

func NewCSharpSymbolTable() *CSharpSymbolTable {
	st := &CSharpSymbolTable{
		knownTypeNames: make(map[string]bool),
		genericArities: make(map[string]map[int]bool),
	}
	st.GlobalScope = newCSharpScope(CSharpScopeKindGlobal, nil, "<global>")
	st.scopeStack = append(st.scopeStack, st.GlobalScope)
	st.populateBuiltins()
	return st
}

func (st *CSharpSymbolTable) CurrentScope() *CSharpScope {
	return st.scopeStack[len(st.scopeStack)-1]
}

func (st *CSharpSymbolTable) EnterScope(kind CSharpScopeKind) {
	st.scopeStack = append(st.scopeStack, newCSharpScope(kind, st.CurrentScope(), ""))
}

func (st *CSharpSymbolTable) ExitScope() {
	if len(st.scopeStack) > 1 {
		st.scopeStack = st.scopeStack[:len(st.scopeStack)-1]
	}
}

func (st *CSharpSymbolTable) DeclareType(name string, kind CSharpTypeKind, arity int) {
	sym := &CSharpSymbol{Name: name, Kind: CSharpSymbolKindType, TypeKind: kind}
	st.CurrentScope().Declare(sym)
	st.knownTypeNames[name] = true
	if arity > 0 {
		if st.genericArities[name] == nil {
			st.genericArities[name] = make(map[int]bool)
		}
		st.genericArities[name][arity] = true
	}
}

func (st *CSharpSymbolTable) DeclareTypeParam(name string) {
	st.CurrentScope().Declare(&CSharpSymbol{Name: name, Kind: CSharpSymbolKindTypeParameter})
	st.knownTypeNames[name] = true
}

func (st *CSharpSymbolTable) DeclareVariable(name, typeRef string) {
	st.CurrentScope().Declare(&CSharpSymbol{Name: name, Kind: CSharpSymbolKindVariable, TypeRef: typeRef})
}

func (st *CSharpSymbolTable) DeclareAlias(alias, target string) {
	st.CurrentScope().Declare(&CSharpSymbol{Name: alias, Kind: CSharpSymbolKindAlias, Target: target})
	st.knownTypeNames[alias] = true
}

func (st *CSharpSymbolTable) ImportNamespace(ns string) {
	st.CurrentScope().Declare(&CSharpSymbol{Name: ns, Kind: CSharpSymbolKindNamespace})
}

func (st *CSharpSymbolTable) IsTypeName(name string) bool {
	if sym := st.CurrentScope().LookupChain(name); sym != nil {
		return sym.Kind == CSharpSymbolKindType || sym.Kind == CSharpSymbolKindTypeParameter || sym.Kind == CSharpSymbolKindAlias
	}
	return st.knownTypeNames[name]
}

var gocsReserved = map[string]bool{
	"abstract": true, "as": true, "base": true, "bool": true, "break": true, "byte": true,
	"case": true, "catch": true, "char": true, "checked": true, "class": true, "const": true,
	"continue": true, "decimal": true, "default": true, "delegate": true, "do": true,
	"double": true, "else": true, "enum": true, "event": true, "explicit": true, "extern": true,
	"false": true, "finally": true, "fixed": true, "float": true, "for": true, "foreach": true,
	"goto": true, "if": true, "implicit": true, "in": true, "int": true, "interface": true,
	"internal": true, "is": true, "lock": true, "long": true, "namespace": true, "new": true,
	"null": true, "object": true, "operator": true, "out": true, "override": true, "params": true,
	"private": true, "protected": true, "public": true, "readonly": true, "ref": true,
	"return": true, "sbyte": true, "sealed": true, "short": true, "sizeof": true,
	"stackalloc": true, "static": true, "string": true, "struct": true, "switch": true,
	"this": true, "throw": true, "true": true, "try": true, "typeof": true, "uint": true,
	"ulong": true, "unchecked": true, "unsafe": true, "ushort": true, "using": true,
	"virtual": true, "void": true, "volatile": true, "while": true,
}

func gocsIsIdentLike(t string) bool {
	if len(t) == 0 {
		return false
	}
	c := rune(t[0])
	return unicode.IsLetter(c) || c == '_' || c == '@'
}

func (st *CSharpSymbolTable) nextDefault(tokens antlr.TokenStream, pos, n int) int {
	for pos < n && tokens.Get(pos).GetChannel() != 0 {
		pos++
	}
	return pos
}

func (st *CSharpSymbolTable) skipAngled(tokens antlr.TokenStream, pos, n int) int {
	depth := 1
	i := st.nextDefault(tokens, pos+1, n)
	for i < n && depth > 0 {
		t := tokens.Get(i).GetText()
		if t == "<" {
			depth++
		} else if t == ">" {
			depth--
		}
		i = st.nextDefault(tokens, i+1, n)
	}
	return i
}

func (st *CSharpSymbolTable) countTypeParams(tokens antlr.TokenStream, pos, n int) (int, []string) {
	p := st.nextDefault(tokens, pos, n)
	if p >= n || tokens.Get(p).GetText() != "<" {
		return 0, nil
	}
	depth, arity := 1, 1
	var names []string
	i := st.nextDefault(tokens, p+1, n)
	for i < n && depth > 0 {
		t := tokens.Get(i).GetText()
		switch {
		case t == "<":
			depth++
		case t == ">":
			depth--
		case t == "," && depth == 1:
			arity++
		case depth == 1 && gocsIsIdentLike(t):
			names = append(names, t)
		}
		i = st.nextDefault(tokens, i+1, n)
	}
	if depth == 0 {
		return arity, names
	}
	return 0, nil
}

func (st *CSharpSymbolTable) registerType(name string, arity int, typeParams []string) {
	if !gocsReserved[name] {
		st.knownTypeNames[name] = true
	}
	if arity > 0 && !gocsReserved[name] {
		if st.genericArities[name] == nil {
			st.genericArities[name] = make(map[int]bool)
		}
		st.genericArities[name][arity] = true
	}
	for _, tp := range typeParams {
		if gocsIsIdentLike(tp) && !gocsReserved[tp] {
			st.knownTypeNames[tp] = true
		}
	}
}

func (st *CSharpSymbolTable) PreScan(tokens antlr.TokenStream) {
	if bts, ok := tokens.(interface{ Fill() }); ok {
		bts.Fill()
	}
	n := tokens.Size()
	for i := 0; i < n; i++ {
		tok := tokens.Get(i)
		if tok.GetChannel() != 0 {
			continue
		}
		txt := tok.GetText()
		switch txt {
		case "class", "struct", "interface", "enum":
			j := st.nextDefault(tokens, i+1, n)
			for j < n && (tokens.Get(j).GetText() == "partial" || tokens.Get(j).GetText() == "ref") {
				j = st.nextDefault(tokens, j+1, n)
			}
			if j < n && gocsIsIdentLike(tokens.Get(j).GetText()) {
				typeName := tokens.Get(j).GetText()
				arity, typeParams := st.countTypeParams(tokens, j+1, n)
				st.registerType(typeName, arity, typeParams)
			}
		case "delegate":
			j := st.nextDefault(tokens, i+1, n)
			for j < n && (tokens.Get(j).GetText() == "ref" || tokens.Get(j).GetText() == "readonly") {
				j = st.nextDefault(tokens, j+1, n)
			}
			if j >= n {
				continue
			}
			j = st.nextDefault(tokens, j+1, n)
			if j < n && tokens.Get(j).GetText() == "<" {
				j = st.skipAngled(tokens, j, n)
			}
			for j < n && (tokens.Get(j).GetText() == "[" || tokens.Get(j).GetText() == "]" || tokens.Get(j).GetText() == ",") {
				j = st.nextDefault(tokens, j+1, n)
			}
			if j < n && tokens.Get(j).GetText() == "?" {
				j = st.nextDefault(tokens, j+1, n)
			}
			if j < n && gocsIsIdentLike(tokens.Get(j).GetText()) {
				typeName := tokens.Get(j).GetText()
				arity, typeParams := st.countTypeParams(tokens, j+1, n)
				st.registerType(typeName, arity, typeParams)
			}
		}
	}
}

func (st *CSharpSymbolTable) populateBuiltins() {
	builtins := []string{
		"bool", "byte", "sbyte", "char", "decimal", "double", "float", "int", "uint",
		"long", "ulong", "short", "ushort", "object", "string", "void", "dynamic",
		"Boolean", "Byte", "SByte", "Char", "Decimal", "Double", "Single",
		"Int16", "Int32", "Int64", "UInt16", "UInt32", "UInt64", "IntPtr", "UIntPtr",
		"Object", "String", "Void", "Guid", "DateTime", "DateTimeOffset", "TimeSpan", "Uri",
		"Exception", "Type", "Enum", "Delegate", "MulticastDelegate", "Attribute",
		"Math", "Convert", "Console", "Environment",
		"List", "IList", "IEnumerable", "IEnumerator", "ICollection",
		"IReadOnlyList", "IReadOnlyCollection", "IReadOnlyDictionary",
		"Dictionary", "IDictionary", "SortedDictionary", "SortedList",
		"HashSet", "SortedSet", "Queue", "Stack", "LinkedList", "LinkedListNode",
		"Func", "Action", "Predicate", "Comparison", "Converter",
		"Task", "ValueTask", "Nullable", "IAsyncEnumerable", "IAsyncEnumerator",
		"Span", "ReadOnlySpan", "Memory", "ReadOnlyMemory", "KeyValuePair", "Tuple",
		"ImmutableArray", "ImmutableList", "ImmutableDictionary", "ImmutableHashSet",
		"Lazy", "WeakReference", "EventHandler", "IEqualityComparer", "IComparer",
		"EqualityComparer", "Comparer", "ConcurrentDictionary", "ConcurrentQueue",
		"ConcurrentStack", "ConcurrentBag",
	}
	for _, t := range builtins {
		st.knownTypeNames[t] = true
	}
}

// =============================================================================
// CSharpParserBase — symbol table field and predicate/action methods
// =============================================================================

// GetSymTable returns the symbol table, creating it lazily if needed.
func (p *CSharpParserBase) GetSymTable() *CSharpSymbolTable {
	if p.symTable == nil {
		p.symTable = NewCSharpSymbolTable()
		p.pendingVarType = "?"
	}
	return p.symTable
}

// EnterNamespaceScope pushes a namespace scope.
func (p *CSharpParserBase) EnterNamespaceScope() { p.GetSymTable().EnterScope(CSharpScopeKindNamespace) }

// EnterTypeScope pushes a type scope.
func (p *CSharpParserBase) EnterTypeScope() { p.GetSymTable().EnterScope(CSharpScopeKindType) }

// EnterBlockScope pushes a block scope.
func (p *CSharpParserBase) EnterBlockScope() { p.GetSymTable().EnterScope(CSharpScopeKindBlock) }

// ExitCurrentScope pops the current scope.
func (p *CSharpParserBase) ExitCurrentScope() { p.GetSymTable().ExitScope() }

// goGetChildText gets text of child idx from a parser rule context.
func goGetChildText(ctx antlr.ParserRuleContext, idx int) string {
	if ctx == nil || idx >= ctx.GetChildCount() {
		return ""
	}
	child := ctx.GetChild(idx)
	if child == nil {
		return ""
	}
	// Try terminal node (token)
	if tn, ok := child.(antlr.TerminalNode); ok {
		return tn.GetText()
	}
	// Try rule context
	if rc, ok := child.(antlr.RuleContext); ok {
		return rc.GetText()
	}
	return ""
}

// OnTypeParameter registers the last child of the current context as a type parameter.
func (p *CSharpParserBase) OnTypeParameter() {
	ctx := p.GetParserRuleContext()
	if ctx == nil {
		return
	}
	n := ctx.GetChildCount()
	if n == 0 {
		return
	}
	name := goGetChildText(ctx, n-1)
	p.GetSymTable().DeclareTypeParam(name)
}

// OnUsingAliasDirective registers a using-alias directive.
func (p *CSharpParserBase) OnUsingAliasDirective() {
	ctx := p.GetParserRuleContext()
	if ctx == nil || ctx.GetChildCount() < 4 {
		return
	}
	alias  := goGetChildText(ctx, 1)
	target := goGetChildText(ctx, 3)
	p.GetSymTable().DeclareAlias(alias, target)
}

// OnUsingNamespaceDirective registers a using-namespace directive.
func (p *CSharpParserBase) OnUsingNamespaceDirective() {
	ctx := p.GetParserRuleContext()
	if ctx == nil || ctx.GetChildCount() < 2 {
		return
	}
	ns := goGetChildText(ctx, 1)
	p.GetSymTable().ImportNamespace(ns)
}

// BeginVariableDeclaration captures the type text for the pending variable declaration.
func (p *CSharpParserBase) BeginVariableDeclaration() {
	ctx := p.GetParserRuleContext()
	if ctx == nil {
		return
	}
	n := ctx.GetChildCount()
	if n == 0 {
		return
	}
	p.pendingVarType = goGetChildText(ctx, n-1)
}

// OnVariableDeclarator declares a variable with the pending type.
func (p *CSharpParserBase) OnVariableDeclarator() {
	ctx := p.GetParserRuleContext()
	if ctx == nil {
		return
	}
	n := ctx.GetChildCount()
	if n == 0 {
		return
	}
	id := goGetChildText(ctx, n-1)
	p.GetSymTable().DeclareVariable(id, p.pendingVarType)
}

// IsCastExpressionAhead returns true if LT(2) is not a known variable.
func (p *CSharpParserBase) IsCastExpressionAhead() bool {
	tok := p.GetTokenStream().LT(2)
	if tok == nil {
		return true
	}
	sym := p.GetSymTable().CurrentScope().LookupChain(tok.GetText())
	return sym == nil || sym.Kind != CSharpSymbolKindVariable
}

// IsTypeParameterName returns true if LT(1) is a known type parameter.
func (p *CSharpParserBase) IsTypeParameterName() bool {
	tok := p.GetTokenStream().LT(1)
	if tok == nil {
		return false
	}
	sym := p.GetSymTable().CurrentScope().LookupChain(tok.GetText())
	return sym != nil && sym.Kind == CSharpSymbolKindTypeParameter
}

// IsValueTypeName returns true if LT(1) is a value-type keyword or a known struct/enum.
func (p *CSharpParserBase) IsValueTypeName() bool {
	tok := p.GetTokenStream().LT(1)
	if tok == nil {
		return false
	}
	switch tok.GetTokenType() {
	case CSharpLexerKW_BOOL, CSharpLexerKW_BYTE, CSharpLexerKW_CHAR,
		CSharpLexerKW_DECIMAL, CSharpLexerKW_DOUBLE, CSharpLexerKW_FLOAT,
		CSharpLexerKW_INT, CSharpLexerKW_LONG, CSharpLexerKW_SBYTE,
		CSharpLexerKW_SHORT, CSharpLexerKW_UINT, CSharpLexerKW_ULONG,
		CSharpLexerKW_USHORT, CSharpLexerTK_LPAREN:
		return true
	}
	sym := p.GetSymTable().CurrentScope().LookupChain(tok.GetText())
	if sym == nil || sym.Kind != CSharpSymbolKindType {
		return false
	}
	return sym.TypeKind == CSharpTypeKindStruct || sym.TypeKind == CSharpTypeKindEnum
}

// IsReferenceTypeName returns true if LT(1) is a reference-type opener or a known class/interface/delegate.
func (p *CSharpParserBase) IsReferenceTypeName() bool {
	tok := p.GetTokenStream().LT(1)
	if tok == nil {
		return true
	}
	switch tok.GetTokenType() {
	case CSharpLexerKW_DYNAMIC, CSharpLexerKW_OBJECT, CSharpLexerKW_STRING, CSharpLexerTK_LBRACK:
		return true
	}
	sym := p.GetSymTable().CurrentScope().LookupChain(tok.GetText())
	if sym == nil {
		return true
	}
	if sym.Kind == CSharpSymbolKindTypeParameter {
		return false
	}
	if sym.Kind == CSharpSymbolKindType && (sym.TypeKind == CSharpTypeKindStruct || sym.TypeKind == CSharpTypeKindEnum) {
		return false
	}
	return true
}

// IsDelegateTypeName returns true if LT(1) is a known delegate type.
func (p *CSharpParserBase) IsDelegateTypeName() bool {
	tok := p.GetTokenStream().LT(1)
	if tok == nil {
		return false
	}
	sym := p.GetSymTable().CurrentScope().LookupChain(tok.GetText())
	return sym != nil && sym.Kind == CSharpSymbolKindType && sym.TypeKind == CSharpTypeKindDelegate
}

// IsInterfaceTypeName returns true if LT(1) is a known interface type.
func (p *CSharpParserBase) IsInterfaceTypeName() bool {
	tok := p.GetTokenStream().LT(1)
	if tok == nil {
		return false
	}
	sym := p.GetSymTable().CurrentScope().LookupChain(tok.GetText())
	return sym != nil && sym.Kind == CSharpSymbolKindType && sym.TypeKind == CSharpTypeKindInterface
}

// IsClassTypeName returns true if LT(1) is (or defaults to) a class type.
func (p *CSharpParserBase) IsClassTypeName() bool {
	tok := p.GetTokenStream().LT(1)
	if tok == nil {
		return true
	}
	switch tok.GetTokenType() {
	case CSharpLexerKW_OBJECT, CSharpLexerKW_STRING:
		return true
	}
	sym := p.GetSymTable().CurrentScope().LookupChain(tok.GetText())
	if sym == nil {
		return true
	}
	if sym.Kind != CSharpSymbolKindType {
		return true
	}
	return sym.TypeKind != CSharpTypeKindInterface && sym.TypeKind != CSharpTypeKindDelegate
}

func (p *CSharpParserBase) classBaseTypeCheck(wantInterface bool) bool {
	tok := p.GetTokenStream().LT(2)
	if tok == nil {
		return !wantInterface
	}
	switch tok.GetTokenType() {
	case CSharpLexerKW_OBJECT, CSharpLexerKW_STRING:
		return !wantInterface
	}
	sym := p.GetSymTable().CurrentScope().LookupChain(tok.GetText())
	if sym == nil || sym.Kind != CSharpSymbolKindType {
		return !wantInterface
	}
	isIface := sym.TypeKind == CSharpTypeKindInterface
	if wantInterface {
		return isIface
	}
	return !isIface
}

// IsClassBaseInterfaceList returns true if LT(2) is a known interface type.
func (p *CSharpParserBase) IsClassBaseInterfaceList() bool { return p.classBaseTypeCheck(true) }

// IsClassBaseClassType returns true if LT(2) is not a known interface type.
func (p *CSharpParserBase) IsClassBaseClassType() bool { return p.classBaseTypeCheck(false) }

// IsDeclarationPatternAhead uses a heuristic token lookahead to check if a declaration pattern follows.
// (Speculative parsing via NewCSharpParser(ts) is unsafe because BaseParser.SetInputStream calls reset()
// which calls ts.Seek(0), corrupting the outer parser's stream position.)
func (p *CSharpParserBase) IsDeclarationPatternAhead() bool {
	ts := p.GetTokenStream()
	valueTypeTokens := map[int]bool{
		CSharpLexerKW_BOOL: true, CSharpLexerKW_BYTE: true, CSharpLexerKW_CHAR: true,
		CSharpLexerKW_DECIMAL: true, CSharpLexerKW_DOUBLE: true, CSharpLexerKW_FLOAT: true,
		CSharpLexerKW_INT: true, CSharpLexerKW_LONG: true, CSharpLexerKW_SBYTE: true,
		CSharpLexerKW_SHORT: true, CSharpLexerKW_UINT: true, CSharpLexerKW_ULONG: true,
		CSharpLexerKW_USHORT: true,
	}
	tok1 := ts.LT(1)
	if tok1 == nil {
		return false
	}
	if valueTypeTokens[tok1.GetTokenType()] {
		tok2 := ts.LT(2)
		return tok2 != nil && (tok2.GetTokenType() == CSharpLexerSimple_Identifier || tok2.GetText() == "_")
	}
	if tok1.GetTokenType() != CSharpLexerSimple_Identifier {
		return false
	}
	sym := p.GetSymTable().CurrentScope().LookupChain(tok1.GetText())
	isKnownType := sym != nil && sym.Kind == CSharpSymbolKindType
	if !isKnownType {
		return false
	}
	tok2 := ts.LT(2)
	return tok2 != nil && (tok2.GetTokenType() == CSharpLexerSimple_Identifier || tok2.GetText() == "_")
}

// IsConstantPatternAhead is the complement of IsDeclarationPatternAhead.
func (p *CSharpParserBase) IsConstantPatternAhead() bool { return !p.IsDeclarationPatternAhead() }

// IsImplicitlyTypedLocalVariable returns true when LT(1)='var' and context implies implicit typing.
func (p *CSharpParserBase) IsImplicitlyTypedLocalVariable() bool {
	tok := p.GetTokenStream().LT(1)
	if tok == nil {
		return true
	}
	if tok.GetTokenType() != CSharpLexerKW_VAR {
		return false
	}
	sym := p.GetSymTable().CurrentScope().LookupChain("var")
	if sym != nil && sym.Kind == CSharpSymbolKindType {
		return false
	}
	lt3 := p.GetTokenStream().LT(3)
	if lt3 == nil || lt3.GetText() != "=" {
		return false
	}
	lt4 := p.GetTokenStream().LT(4)
	if lt4 != nil && lt4.GetText() == "{" {
		return false
	}
	return true
}

// IsExplicitlyTypedLocalVariable returns true when the local variable is explicitly typed.
func (p *CSharpParserBase) IsExplicitlyTypedLocalVariable() bool {
	tok := p.GetTokenStream().LT(1)
	if tok == nil {
		return true
	}
	if tok.GetTokenType() != CSharpLexerKW_VAR {
		return true
	}
	sym := p.GetSymTable().CurrentScope().LookupChain("var")
	if sym != nil && sym.Kind == CSharpSymbolKindType {
		return true
	}
	lt3 := p.GetTokenStream().LT(3)
	if lt3 == nil || lt3.GetText() != "=" {
		return true
	}
	lt4 := p.GetTokenStream().LT(4)
	if lt4 != nil && lt4.GetText() == "{" {
		return true
	}
	return false
}

// IsExplicitlyTypedRefLocalVariable returns true when LT(1) is 'ref'.
func (p *CSharpParserBase) IsExplicitlyTypedRefLocalVariable() bool {
	tok := p.GetTokenStream().LT(1)
	return tok != nil && tok.GetTokenType() == CSharpLexerKW_REF
}

// IsLocalVariableDeclaration gates comma-separated declarators.
// Returns false when the type is 'var' (only one declarator allowed).
func (p *CSharpParserBase) IsLocalVariableDeclaration() bool {
	ctx := p.GetParserRuleContext()
	local_var_decl, ok := ctx.(*Local_variable_declarationContext)
	if !ok || local_var_decl == nil {
		return true
	}
	local_variable_type := local_var_decl.Local_variable_type()
	if local_variable_type == nil {
		return true
	}
	return local_variable_type.GetText() != "var"
}
