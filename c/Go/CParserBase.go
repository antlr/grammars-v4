package parser

import (
	"fmt"
	"os"
	"strconv"
	"strings"

	"github.com/antlr4-go/antlr/v4"
)

var allSemanticFunctions = []string{
	"IsAlignmentSpecifier", "IsAtomicTypeSpecifier", "IsAttributeDeclaration",
	"IsAttributeSpecifier", "IsAttributeSpecifierSequence", "IsDeclaration",
	"IsDeclarationSpecifier", "IsTypeSpecifierQualifier", "IsEnumSpecifier",
	"IsFunctionSpecifier", "IsStatement", "IsStaticAssertDeclaration",
	"IsStorageClassSpecifier", "IsStructOrUnionSpecifier", "IsTypedefName",
	"IsTypeofSpecifier", "IsTypeQualifier", "IsTypeSpecifier", "IsCast",
	"IsNullStructDeclarationListExtension",
	"IsGnuAttributeBeforeDeclarator",
	"IsSomethingOfTypeName", "IsSpecifierQualifierList", "IsTypeName",
	"IsInitDeclaratorList",
}

// CParserBase holds the semantic state shared by all parser actions.
type CParserBase struct {
	*antlr.BaseParser
	st                       *SymbolTable
	debug                    bool
	outputSymbolTable        bool
	outputAppliedOccurrences bool
	noSemantics              map[string]bool
	initialized              bool
}

func (p *CParserBase) initSemantics() {
	if p.initialized {
		return
	}
	p.initialized = true
	p.st = NewSymbolTable()
	p.noSemantics = make(map[string]bool)
	for _, arg := range os.Args {
		lower := strings.ToLower(arg)
		if strings.Contains(lower, "--debug") {
			p.debug = true
		}
		if strings.Contains(lower, "--output-symbol-table") {
			p.outputSymbolTable = true
		}
		if strings.Contains(lower, "--output-applied-occurrences") {
			p.outputAppliedOccurrences = true
		}
		if strings.HasPrefix(lower, "--no-semantics") {
			eq := strings.Index(arg, "=")
			if eq == -1 {
				for _, f := range allSemanticFunctions {
					p.noSemantics[f] = true
				}
			} else {
				for _, f := range strings.Split(arg[eq+1:], ",") {
					p.noSemantics[strings.TrimSpace(f)] = true
				}
			}
		}
	}
}

func (p *CParserBase) IsAlignmentSpecifier(k ...int) bool {
	p.initSemantics()
	if p.noSemantics["IsAlignmentSpecifier"] {
		return true
	}
	ki := 1
	if len(k) > 0 {
		ki = k[0]
	}
	lt1 := p.GetTokenStream().LT(ki)
	if p.debug {
		fmt.Print("IsAlignmentSpecifier ", lt1)
	}
	resolved := p.resolveWithOutput(lt1)
	result := resolved != nil && resolved.Classification[AlignmentSpecifier_]
	if p.debug {
		fmt.Println("", result)
	}
	return result
}

func (p *CParserBase) IsAtomicTypeSpecifier(k ...int) bool {
	p.initSemantics()
	if p.noSemantics["IsAtomicTypeSpecifier"] {
		return true
	}
	ki := 1
	if len(k) > 0 {
		ki = k[0]
	}
	lt1 := p.GetTokenStream().LT(ki)
	if p.debug {
		fmt.Print("IsAtomicTypeSpecifier ", lt1)
	}
	resolved := p.resolveWithOutput(lt1)
	result := resolved != nil && resolved.Classification[AtomicTypeSpecifier_]
	if p.debug {
		fmt.Println("", result)
	}
	return result
}

func (p *CParserBase) IsAttributeDeclaration() bool {
	p.initSemantics()
	if p.noSemantics["IsAttributeDeclaration"] {
		return true
	}
	return p.IsAttributeSpecifierSequence()
}

func (p *CParserBase) IsAttributeSpecifier() bool {
	p.initSemantics()
	if p.noSemantics["IsAttributeSpecifier"] {
		return true
	}
	lt1 := p.GetTokenStream().LT(1)
	if p.debug {
		fmt.Print("IsAttributeSpecifier ", lt1)
	}
	result := lt1.GetTokenType() == CLexerLeftBracket
	if p.debug {
		fmt.Println("", result)
	}
	return result
}

func (p *CParserBase) IsAttributeSpecifierSequence() bool {
	p.initSemantics()
	if p.noSemantics["IsAttributeSpecifierSequence"] {
		return true
	}
	return p.IsAttributeSpecifier()
}

func (p *CParserBase) IsDeclaration() bool {
	p.initSemantics()
	if p.noSemantics["IsDeclaration"] {
		return true
	}
	if p.debug {
		fmt.Println("IsDeclaration")
	}
	result := p.isDeclarationSpecifiers() ||
		p.IsAttributeSpecifierSequence() ||
		p.IsStaticAssertDeclaration() ||
		p.IsAttributeDeclaration()
	if p.debug {
		fmt.Println("IsDeclaration", result)
	}
	return result
}

func (p *CParserBase) isDeclarationSpecifiers() bool {
	return p.IsDeclarationSpecifier()
}

func (p *CParserBase) IsDeclarationSpecifier() bool {
	p.initSemantics()
	if p.noSemantics["IsDeclarationSpecifier"] {
		return true
	}
	lt1 := p.GetTokenStream().LT(1)
	if p.debug {
		fmt.Println("IsDeclarationSpecifier", lt1)
	}
	result := p.IsStorageClassSpecifier() ||
		p.IsTypeSpecifier() ||
		p.IsTypeQualifier() ||
		(p.IsFunctionSpecifier() && !p.IsGnuAttributeBeforeDeclarator()) ||
		p.IsAlignmentSpecifier()
	if p.debug {
		fmt.Println("IsDeclarationSpecifier", result, "for", lt1)
	}
	return result
}

func (p *CParserBase) IsTypeSpecifierQualifier(k ...int) bool {
	p.initSemantics()
	if p.noSemantics["IsTypeSpecifierQualifier"] {
		return true
	}
	if p.debug {
		fmt.Println("IsTypeSpecifierQualifier")
	}
	ki := 1
	if len(k) > 0 {
		ki = k[0]
	}
	result := p.IsTypeSpecifier(ki) || p.IsTypeQualifier(ki) || p.IsAlignmentSpecifier(ki)
	if p.debug {
		fmt.Println("IsTypeSpecifierQualifier", result)
	}
	return result
}

func (p *CParserBase) IsEnumSpecifier(k ...int) bool {
	p.initSemantics()
	if p.noSemantics["IsEnumSpecifier"] {
		return true
	}
	ki := 1
	if len(k) > 0 {
		ki = k[0]
	}
	lt1 := p.GetTokenStream().LT(ki)
	if p.debug {
		fmt.Print("IsEnumSpecifier ", lt1)
	}
	result := lt1.GetTokenType() == CLexerEnum
	if p.debug {
		fmt.Println("", result)
	}
	return result
}

func (p *CParserBase) IsFunctionSpecifier() bool {
	p.initSemantics()
	if p.noSemantics["IsFunctionSpecifier"] {
		return true
	}
	lt1 := p.GetTokenStream().LT(1)
	if p.debug {
		fmt.Print("IsFunctionSpecifier ", lt1)
	}
	resolved := p.resolveWithOutput(lt1)
	result := resolved != nil && resolved.Classification[FunctionSpecifier_]
	if p.debug {
		fmt.Println("IsFunctionSpecifier", result)
	}
	return result
}

func (p *CParserBase) IsGnuAttributeBeforeDeclarator(k ...int) bool {
	p.initSemantics()
	if p.noSemantics["IsGnuAttributeBeforeDeclarator"] {
		return true
	}
	ki := 1
	if len(k) > 0 {
		ki = k[0]
	}
	ts := p.GetTokenStream()
	i := ki
	if ts.LT(i).GetTokenType() != CLexerAttribute {
		return false
	}
	i++
	depth := 0
	for {
		t := ts.LT(i)
		i++
		if t.GetTokenType() == antlr.TokenEOF {
			return false
		}
		if t.GetTokenType() == CLexerLeftParen {
			depth++
		} else if t.GetTokenType() == CLexerRightParen {
			depth--
			if depth == 0 {
				break
			}
		}
	}
	next := ts.LT(i).GetTokenType()
	return next == CLexerIdentifier || next == CLexerStar || next == CLexerLeftParen
}

func (p *CParserBase) IsStatement() bool {
	p.initSemantics()
	if p.noSemantics["IsStatement"] {
		return true
	}
	t1 := p.GetTokenStream().LT(1)
	t2 := p.GetTokenStream().LT(2)
	if p.debug {
		fmt.Println("IsStatement1", t1)
		fmt.Println("IsStatement2", t2)
	}
	if t1.GetTokenType() == CLexerIdentifier && t2.GetTokenType() == CLexerColon {
		if p.debug {
			fmt.Print("IsStatement3 true")
		}
		return true
	}
	result := !p.IsDeclaration()
	if p.debug {
		fmt.Print("IsStatement", result)
	}
	return result
}

func (p *CParserBase) IsStaticAssertDeclaration() bool {
	p.initSemantics()
	if p.noSemantics["IsStaticAssertDeclaration"] {
		return true
	}
	token := p.GetTokenStream().LT(1)
	if p.debug {
		fmt.Print("IsStaticAssertDeclaration ", token)
	}
	result := token.GetTokenType() == CLexerStatic_assert
	if p.debug {
		fmt.Println("", result)
	}
	return result
}

func (p *CParserBase) IsStorageClassSpecifier() bool {
	p.initSemantics()
	if p.noSemantics["IsStorageClassSpecifier"] {
		return true
	}
	lt1 := p.GetTokenStream().LT(1)
	if p.debug {
		fmt.Print("IsStorageClassSpecifier ", lt1)
	}
	resolved := p.resolveWithOutput(lt1)
	result := resolved != nil && resolved.Classification[StorageClassSpecifier_]
	if p.debug {
		fmt.Println("", result)
	}
	return result
}

func (p *CParserBase) IsStructOrUnionSpecifier(k ...int) bool {
	p.initSemantics()
	if p.noSemantics["IsStructOrUnionSpecifier"] {
		return true
	}
	ki := 1
	if len(k) > 0 {
		ki = k[0]
	}
	token := p.GetTokenStream().LT(ki)
	if p.debug {
		fmt.Print("IsStructOrUnionSpecifier ", token)
	}
	result := token.GetTokenType() == CLexerStruct || token.GetTokenType() == CLexerUnion
	if p.debug {
		fmt.Println("", result)
	}
	return result
}

func (p *CParserBase) IsTypedefName(k ...int) bool {
	p.initSemantics()
	if p.noSemantics["IsTypedefName"] {
		return true
	}
	ki := 1
	if len(k) > 0 {
		ki = k[0]
	}
	lt1 := p.GetTokenStream().LT(ki)
	if p.debug {
		fmt.Print("IsTypedefName ", lt1)
	}
	resolved := p.resolveWithOutput(lt1)
	var result bool
	if resolved == nil {
		result = false
	} else if resolved.Classification[Variable_] {
		result = false
	} else if resolved.Classification[Function_] {
		result = false
	} else {
		result = true
	}
	if p.debug {
		fmt.Println("", result)
	}
	return result
}

func (p *CParserBase) IsTypeofSpecifier(k ...int) bool {
	p.initSemantics()
	if p.noSemantics["IsTypeofSpecifier"] {
		return true
	}
	ki := 1
	if len(k) > 0 {
		ki = k[0]
	}
	token := p.GetTokenStream().LT(ki)
	if p.debug {
		fmt.Print("IsTypeofSpecifier ", token)
	}
	result := token.GetTokenType() == CLexerTypeof || token.GetTokenType() == CLexerTypeof_unqual
	if p.debug {
		fmt.Println("", result)
	}
	return result
}

func (p *CParserBase) IsTypeQualifier(k ...int) bool {
	p.initSemantics()
	if p.noSemantics["IsTypeQualifier"] {
		return true
	}
	ki := 1
	if len(k) > 0 {
		ki = k[0]
	}
	lt1 := p.GetTokenStream().LT(ki)
	if p.debug {
		fmt.Print("IsTypeQualifier ", lt1)
	}
	resolved := p.resolveWithOutput(lt1)
	result := resolved != nil && resolved.Classification[TypeQualifier_]
	if p.debug {
		fmt.Println("", result)
	}
	return result
}

func (p *CParserBase) IsTypeSpecifier(k ...int) bool {
	p.initSemantics()
	if p.noSemantics["IsTypeSpecifier"] {
		return true
	}
	ki := 1
	if len(k) > 0 {
		ki = k[0]
	}
	lt1 := p.GetTokenStream().LT(ki)
	if p.debug {
		fmt.Print("IsTypeSpecifier ", lt1)
	}
	resolved := p.resolveWithOutput(lt1)
	if resolved != nil && resolved.Classification[TypeSpecifier_] {
		if p.debug {
			fmt.Println("", true)
		}
		return true
	}
	result := p.IsAtomicTypeSpecifier(ki) || p.IsStructOrUnionSpecifier(ki) ||
		p.IsEnumSpecifier(ki) || p.IsTypedefName(ki) || p.IsTypeofSpecifier(ki)
	if p.debug {
		fmt.Println("", result)
	}
	return result
}

func (p *CParserBase) IsNullStructDeclarationListExtension() bool {
	p.initSemantics()
	if p.noSemantics["IsNullStructDeclarationListExtension"] {
		return true
	}
	return true
}

func (p *CParserBase) IsSomethingOfTypeName() bool {
	p.initSemantics()
	if p.noSemantics["IsSomethingOfTypeName"] {
		return true
	}
	ts := p.GetTokenStream()
	t1 := ts.LT(1).GetTokenType()
	if t1 != CLexerSizeof && t1 != CLexerCountof && t1 != CLexerAlignof &&
		t1 != CLexerMaxof && t1 != CLexerMinof {
		return false
	}
	if ts.LT(2).GetTokenType() != CLexerLeftParen {
		return false
	}
	return p.IsTypeName(3)
}

func (p *CParserBase) IsTypeName(k ...int) bool {
	p.initSemantics()
	if p.noSemantics["IsTypeName"] {
		return true
	}
	ki := 1
	if len(k) > 0 {
		ki = k[0]
	}
	return p.IsSpecifierQualifierList(ki)
}

func (p *CParserBase) IsSpecifierQualifierList(k ...int) bool {
	p.initSemantics()
	if p.noSemantics["IsSpecifierQualifierList"] {
		return true
	}
	ki := 1
	if len(k) > 0 {
		ki = k[0]
	}
	if p.IsGnuAttributeBeforeDeclarator(ki) {
		return true
	}
	return p.IsTypeSpecifierQualifier(ki)
}

func (p *CParserBase) IsCast() bool {
	p.initSemantics()
	if p.noSemantics["IsCast"] {
		return true
	}
	t1 := p.GetTokenStream().LT(1)
	t2 := p.GetTokenStream().LT(2)
	if p.debug {
		fmt.Println("IsCast1", t1)
		fmt.Println("IsCast2", t2)
	}
	var result bool
	if t1.GetTokenType() != CLexerLeftParen {
		result = false
	} else if t2.GetTokenType() != CLexerIdentifier {
		result = true
	} else {
		resolved := p.resolveWithOutput(t2)
		if resolved == nil {
			result = false
		} else if resolved.Classification[TypeSpecifier_] {
			result = true
		} else {
			result = false
		}
	}
	if p.debug {
		fmt.Print("IsCast", result)
	}
	return result
}

func (p *CParserBase) IsInitDeclaratorList() bool {
	p.initSemantics()
	if p.noSemantics["IsInitDeclaratorList"] {
		return true
	}
	lt1 := p.GetTokenStream().LT(1)
	if p.debug {
		fmt.Print("IsInitDeclaratorList ", lt1)
	}
	resolved := p.resolveWithOutput(lt1)
	var result bool
	if resolved == nil {
		result = true
	} else if resolved.Classification[TypeQualifier_] ||
		resolved.Classification[TypeSpecifier_] ||
		lt1.GetText() == "__attribute__" {
		result = false
	} else {
		result = true
	}
	if p.debug {
		fmt.Println("", result)
	}
	return result
}

func (p *CParserBase) EnterDeclaration() {
	p.initSemantics()
	if p.debug {
		fmt.Println("EnterDeclaration")
	}
	context := p.GetParserRuleContext()
	for context != nil {
		if declCtx, ok := context.(*DeclarationContext); ok {
			dsCtx := declCtx.DeclarationSpecifiers()
			var declarationSpecifiers []IDeclarationSpecifierContext
			if dsCtx != nil {
				declarationSpecifiers = dsCtx.AllDeclarationSpecifier()
			}
			initDeclList := declCtx.InitDeclaratorList()
			if initDeclList != nil {
				isTypedef := false
				for _, ds := range declarationSpecifiers {
					sc := ds.StorageClassSpecifier()
					if sc != nil && sc.Typedef() != nil {
						isTypedef = true
						break
					}
				}
				for _, id := range initDeclList.AllInitDeclarator() {
					declarator := id.Declarator()
					idToken := p.getDeclarationToken(declarator)
					if idToken != nil {
						text := idToken.GetText()
						loc := p.getSourceLocation(idToken)
						var sym *Symbol
						if isTypedef {
							sym = &Symbol{
								Name:           text,
								Classification: map[TypeClassification]bool{TypeSpecifier_: true},
								Members:        make(map[string]*Symbol),
								DefinedFile:    loc.file,
								DefinedLine:    loc.line,
								DefinedColumn:  loc.column,
							}
						} else {
							sym = &Symbol{
								Name:           text,
								Classification: map[TypeClassification]bool{Variable_: true},
								Members:        make(map[string]*Symbol),
								DefinedFile:    loc.file,
								DefinedLine:    loc.line,
								DefinedColumn:  loc.column,
							}
						}
						p.st.Define(sym)
						if p.debug {
							fmt.Println("New symbol Declaration Declarator", sym)
						}
					}
				}
			}
		}
		if fdCtx, ok := context.(*FunctionDefinitionContext); ok {
			de := fdCtx.Declarator()
			if de != nil {
				dd := de.DirectDeclarator()
				if dd != nil {
					identifier := dd.Identifier()
					if identifier != nil {
						idToken := identifier.GetSymbol()
						text := idToken.GetText()
						loc := p.getSourceLocation(idToken)
						sym := &Symbol{
							Name:           text,
							Classification: map[TypeClassification]bool{Function_: true},
							Members:        make(map[string]*Symbol),
							DefinedFile:    loc.file,
							DefinedLine:    loc.line,
							DefinedColumn:  loc.column,
						}
						p.st.Define(sym)
						if p.debug {
							fmt.Println("New symbol Declarationf Declarator", sym)
						}
						return
					}
				}
			}
		}
		parent := context.GetParent()
		if parent == nil {
			break
		}
		if prc, ok := parent.(antlr.ParserRuleContext); ok {
			context = prc
		} else {
			break
		}
	}
}

func (p *CParserBase) getDeclarationToken(y IDeclaratorContext) antlr.Token {
	if y == nil {
		return nil
	}
	dd := y.DirectDeclarator()
	if dd != nil {
		more := dd.Declarator()
		token := p.getDeclarationToken(more)
		if token != nil {
			return token
		}
		if dd.Identifier() != nil {
			return dd.Identifier().GetSymbol()
		}
	}
	return nil
}

func (p *CParserBase) EnterScope() {
	p.initSemantics()
	if p.debug {
		fmt.Println("EnterScope")
	}
	p.st.PushBlockScope()
}

func (p *CParserBase) ExitScope() {
	p.initSemantics()
	if p.debug {
		fmt.Println("ExitScope")
	}
	p.st.PopBlockScope()
}

func (p *CParserBase) LookupSymbol() {
	p.initSemantics()
	token := p.GetTokenStream().LT(-1)
	if token == nil {
		return
	}
	text := token.GetText()
	resolved := p.st.Resolve(text, nil)
	if p.outputAppliedOccurrences && resolved != nil {
		loc := p.getSourceLocation(token)
		fmt.Fprintf(os.Stderr, "Applied occurrence: %s at %s:%d:%d -> Defined at %s:%d:%d\n",
			text, loc.file, loc.line, loc.column,
			resolved.DefinedFile, resolved.DefinedLine, resolved.DefinedColumn)
	}
}

func (p *CParserBase) OutputSymbolTable() {
	p.initSemantics()
	if p.outputSymbolTable {
		fmt.Fprint(os.Stderr, p.st.String())
	}
}

func (p *CParserBase) resolveWithOutput(token antlr.Token) *Symbol {
	if token == nil {
		return nil
	}
	text := token.GetText()
	resolved := p.st.Resolve(text, nil)
	if p.outputAppliedOccurrences && resolved != nil {
		loc := p.getSourceLocation(token)
		fmt.Fprintf(os.Stderr, "Applied occurrence: %s at %s:%d:%d -> Defined at %s:%d:%d\n",
			text, loc.file, loc.line, loc.column,
			resolved.DefinedFile, resolved.DefinedLine, resolved.DefinedColumn)
	}
	return resolved
}

type sourceLocation struct {
	file   string
	line   int
	column int
}

func (p *CParserBase) getSourceLocation(token antlr.Token) sourceLocation {
	if token == nil {
		return sourceLocation{}
	}
	ts := p.GetTokenStream()
	ind := token.GetTokenIndex()
	line := token.GetLine()
	column := token.GetColumn()
	fileName := "<unknown>"
	lineAdjusted := line

	for j := ind; j >= 0; j-- {
		t := ts.Get(j)
		if t == nil {
			break
		}
		if t.GetTokenType() == CLexerLineDirective {
			txt := t.GetText()
			parts := strings.Fields(txt)
			if len(parts) >= 3 {
				if dirLine, err := strconv.Atoi(parts[1]); err == nil {
					lineDirective := t.GetLine()
					lineDiff := line - lineDirective
					lineAdjusted = lineDiff + dirLine - 1
					fileName = strings.Trim(parts[2], "\"")
				}
			}
			break
		}
	}
	return sourceLocation{file: fileName, line: lineAdjusted, column: column}
}
