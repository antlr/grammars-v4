package parser

import (
	"fmt"
	"os"
	"strings"

	"github.com/antlr4-go/antlr/v4"
)

type AdaParserBase struct {
	*antlr.BaseParser
	st                      *SymbolTable
	expectedTypeStack       []*Symbol
	debug                   bool
	outputSymbolTableFlag   bool
	outputAppliedOccurrences bool
	noSemantics             map[string]bool
	initialized             bool
}

func (p *AdaParserBase) initSemantics() {
	if p.initialized {
		return
	}
	p.initialized = true
	p.st = NewSymbolTable()
	p.noSemantics = make(map[string]bool)
	allFuncs := []string{"IsAggregate", "IsTypeName"}
	for _, arg := range os.Args {
		lower := strings.ToLower(arg)
		if strings.Contains(lower, "--debug") {
			p.debug = true
		}
		if strings.Contains(lower, "--output-symbol-table") {
			p.outputSymbolTableFlag = true
		}
		if strings.Contains(lower, "--output-applied-occurrences") {
			p.outputAppliedOccurrences = true
		}
		if strings.HasPrefix(lower, "--no-semantics") {
			eq := strings.Index(arg, "=")
			if eq == -1 {
				for _, f := range allFuncs {
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

// pragmaTokenSource wraps a list of tokens as a Lexer for CommonTokenStream.
type pragmaTokenSource struct {
	*antlr.BaseLexer
	toks []antlr.Token
	pos  int
}

func (s *pragmaTokenSource) NextToken() antlr.Token {
	if s.pos >= len(s.toks) {
		return s.toks[len(s.toks)-1] // EOF
	}
	t := s.toks[s.pos]
	s.pos++
	return t
}

func (s *pragmaTokenSource) GetATN() *antlr.ATN {
	return nil
}

func (s *pragmaTokenSource) GetSourceName() string {
	return "pragma"
}

func (p *AdaParserBase) IsAggregate() bool {
	p.initSemantics()
	if p.noSemantics["IsAggregate"] {
		return true
	}
	stream := p.GetTokenStream().(*antlr.CommonTokenStream)
	lt1 := stream.LT(1)
	if lt1.GetTokenType() != AdaLexerLP {
		return false
	}
	depth := 0
	for i := 2; ; i++ {
		t := stream.LT(i)
		if t == nil || t.GetTokenType() == antlr.TokenEOF {
			break
		}
		if t.GetTokenType() == AdaLexerLP {
			depth++
		} else if t.GetTokenType() == AdaLexerRP {
			if depth == 0 {
				break
			}
			depth--
		} else if depth == 0 {
			if t.GetTokenType() == AdaLexerCOMMA {
				return true
			}
			if t.GetTokenType() == AdaLexerARROW {
				return true
			}
			if t.GetTokenType() == AdaLexerWITH {
				return true
			}
			if t.GetTokenType() == AdaLexerNULL_ {
				next := stream.LT(i + 1)
				if next != nil && next.GetTokenType() == AdaLexerRECORD {
					return true
				}
			}
		}
	}
	if len(p.expectedTypeStack) > 0 {
		expected := p.expectedTypeStack[len(p.expectedTypeStack)-1]
		if expected != nil && expected.IsComposite {
			return true
		}
	}
	return false
}

func (p *AdaParserBase) IsTypeName() bool {
	p.initSemantics()
	if p.noSemantics["IsTypeName"] {
		return true
	}
	stream := p.GetTokenStream().(*antlr.CommonTokenStream)
	lt1 := stream.LT(1)
	if lt1.GetTokenType() != AdaLexerIDENTIFIER_ {
		return false
	}
	firstName := lt1.GetText()
	resolved := p.st.Resolve(firstName)
	return resolved != nil && resolved.Classification[TypeName_]
}

func (p *AdaParserBase) EnterDeclaration() {
	p.initSemantics()
	ctx := p.GetParserRuleContext()
	for ctx != nil {
		switch c := ctx.(type) {
		case *Full_type_declarationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				isComposite := false
				typeDef := c.Type_definition()
				if typeDef != nil {
					isComposite = typeDef.Record_type_definition() != nil || typeDef.Array_type_definition() != nil
				}
				p.defineSymbol(defId.GetText(), TypeName_, defId.GetStart(), isComposite)
			}
			return
		case *Subtype_declarationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				isComposite := false
				si := c.Subtype_indication()
				if si != nil {
					sm := si.Subtype_mark()
					if sm != nil {
						baseSym := p.st.Resolve(sm.GetText())
						if baseSym != nil {
							isComposite = baseSym.IsComposite
						}
					}
				}
				p.defineSymbol(defId.GetText(), TypeName_, defId.GetStart(), isComposite)
			}
			return
		case *Object_declarationContext:
			defIdList := c.Defining_identifier_list()
			if defIdList != nil {
				for _, defId := range defIdList.AllDefining_identifier() {
					p.defineSymbol(defId.GetText(), ObjectName_, defId.GetStart(), false)
				}
			}
			return
		case *Number_declarationContext:
			defIdList := c.Defining_identifier_list()
			if defIdList != nil {
				for _, defId := range defIdList.AllDefining_identifier() {
					p.defineSymbol(defId.GetText(), ObjectName_, defId.GetStart(), false)
				}
			}
			return
		case *Subprogram_declarationContext:
			p.defineSubprogramFromSpec(c.Subprogram_specification())
			return
		case *Subprogram_bodyContext:
			p.defineSubprogramFromSpec(c.Subprogram_specification())
			return
		case *Package_declarationContext:
			pkgSpec := c.Package_specification()
			if pkgSpec != nil {
				dpun := pkgSpec.Defining_program_unit_name()
				if dpun != nil {
					defId := dpun.Defining_identifier()
					if defId != nil {
						p.defineSymbol(defId.GetText(), PackageName_, defId.GetStart(), false)
					}
				}
			}
			return
		case *Package_bodyContext:
			dpun := c.Defining_program_unit_name()
			if dpun != nil {
				defId := dpun.Defining_identifier()
				if defId != nil {
					p.defineSymbol(defId.GetText(), PackageName_, defId.GetStart(), false)
				}
			}
			return
		case *Exception_declarationContext:
			defIdList := c.Defining_identifier_list()
			if defIdList != nil {
				for _, defId := range defIdList.AllDefining_identifier() {
					p.defineSymbol(defId.GetText(), ExceptionName_, defId.GetStart(), false)
				}
			}
			return
		case *Task_type_declarationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), TypeName_, defId.GetStart(), false)
			}
			return
		case *Single_task_declarationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), ObjectName_, defId.GetStart(), false)
			}
			return
		case *Protected_type_declarationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), TypeName_, defId.GetStart(), false)
			}
			return
		case *Single_protected_declarationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), ObjectName_, defId.GetStart(), false)
			}
			return
		case *Entry_declarationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), SubprogramName_, defId.GetStart(), false)
			}
			return
		case *Component_declarationContext:
			defIdList := c.Defining_identifier_list()
			if defIdList != nil {
				for _, defId := range defIdList.AllDefining_identifier() {
					p.defineSymbol(defId.GetText(), ComponentName_, defId.GetStart(), false)
				}
			}
			return
		case *Incomplete_type_declarationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), TypeName_, defId.GetStart(), false)
			}
			return
		case *Private_type_declarationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), TypeName_, defId.GetStart(), false)
			}
			return
		case *Private_extension_declarationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), TypeName_, defId.GetStart(), true)
			}
			return
		case *Generic_instantiationContext:
			dpuns := c.AllDefining_program_unit_name()
			if len(dpuns) > 0 {
				defId := dpuns[0].Defining_identifier()
				if defId != nil {
					tc := PackageName_
					if c.PROCEDURE() != nil || c.FUNCTION() != nil {
						tc = SubprogramName_
					}
					p.defineSymbol(defId.GetText(), tc, defId.GetStart(), false)
				}
			}
			dds := c.AllDefining_designator()
			if len(dds) > 0 {
				dpun := dds[0].Defining_program_unit_name()
				if dpun != nil {
					defId := dpun.Defining_identifier()
					if defId != nil {
						p.defineSymbol(defId.GetText(), SubprogramName_, defId.GetStart(), false)
					}
				}
			}
			return
		case *Object_renaming_declarationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), ObjectName_, defId.GetStart(), false)
			}
			return
		case *Exception_renaming_declarationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), ExceptionName_, defId.GetStart(), false)
			}
			return
		case *Package_renaming_declarationContext:
			dpun := c.Defining_program_unit_name()
			if dpun != nil {
				defId := dpun.Defining_identifier()
				if defId != nil {
					p.defineSymbol(defId.GetText(), PackageName_, defId.GetStart(), false)
				}
			}
			return
		case *Formal_complete_type_declarationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), TypeName_, defId.GetStart(), false)
			}
			return
		case *Formal_incomplete_type_declarationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), TypeName_, defId.GetStart(), false)
			}
			return
		case *Formal_object_declarationContext:
			defIdList := c.Defining_identifier_list()
			if defIdList != nil {
				for _, defId := range defIdList.AllDefining_identifier() {
					p.defineSymbol(defId.GetText(), ObjectName_, defId.GetStart(), false)
				}
			}
			return
		case *Formal_package_declarationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), PackageName_, defId.GetStart(), false)
			}
			return
		case *Parameter_specificationContext:
			defIdList := c.Defining_identifier_list()
			if defIdList != nil {
				for _, defId := range defIdList.AllDefining_identifier() {
					p.defineSymbol(defId.GetText(), ObjectName_, defId.GetStart(), false)
				}
			}
			return
		case *Loop_parameter_specificationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), ObjectName_, defId.GetStart(), false)
			}
			return
		case *Iterator_specificationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), ObjectName_, defId.GetStart(), false)
			}
			return
		case *Enumeration_literal_specificationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), EnumerationLiteral_, defId.GetStart(), false)
			}
			return
		case *Choice_parameter_specificationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), ObjectName_, defId.GetStart(), false)
			}
			return
		case *Entry_index_specificationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), ObjectName_, defId.GetStart(), false)
			}
			return
		case *Extended_return_object_declarationContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), ObjectName_, defId.GetStart(), false)
			}
			return
		case *Entry_bodyContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), SubprogramName_, defId.GetStart(), false)
			}
			return
		case *Task_bodyContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), ObjectName_, defId.GetStart(), false)
			}
			return
		case *Protected_bodyContext:
			defId := c.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), ObjectName_, defId.GetStart(), false)
			}
			return
		case *Discriminant_specificationContext:
			defIdList := c.Defining_identifier_list()
			if defIdList != nil {
				for _, defId := range defIdList.AllDefining_identifier() {
					p.defineSymbol(defId.GetText(), ObjectName_, defId.GetStart(), false)
				}
			}
			return
		}
		parent := ctx.GetParent()
		if parent == nil {
			break
		}
		if prc, ok := parent.(antlr.ParserRuleContext); ok {
			ctx = prc
		} else {
			break
		}
	}
}

func (p *AdaParserBase) defineSubprogramFromSpec(spec ISubprogram_specificationContext) {
	if spec == nil {
		return
	}
	procSpec := spec.Procedure_specification()
	if procSpec != nil {
		dpun := procSpec.Defining_program_unit_name()
		if dpun != nil {
			defId := dpun.Defining_identifier()
			if defId != nil {
				p.defineSymbol(defId.GetText(), SubprogramName_, defId.GetStart(), false)
			}
		}
		return
	}
	funcSpec := spec.Function_specification()
	if funcSpec != nil {
		dd := funcSpec.Defining_designator()
		if dd != nil {
			dpun := dd.Defining_program_unit_name()
			if dpun != nil {
				defId := dpun.Defining_identifier()
				if defId != nil {
					p.defineSymbol(defId.GetText(), SubprogramName_, defId.GetStart(), false)
				}
			}
		}
	}
}

func (p *AdaParserBase) defineSymbol(name string, classification TypeClassification, token antlr.Token, isComposite bool) {
	sym := NewSymbol()
	sym.Name = name
	sym.Classification[classification] = true
	sym.IsComposite = isComposite
	if token != nil && token.GetSource() != nil {
		if token.GetSource().GetTokenSource() != nil {
			sym.DefinedFile = token.GetSource().GetTokenSource().GetSourceName()
		}
		sym.DefinedLine = token.GetLine()
		sym.DefinedColumn = token.GetColumn()
	}
	p.st.Define(sym)
}

func (p *AdaParserBase) EnterScope() {
	p.initSemantics()
	p.st.PushBlockScope()
}

func (p *AdaParserBase) ExitScope() {
	p.initSemantics()
	p.st.PopBlockScope()
}

func (p *AdaParserBase) PushExpectedType() {
	p.initSemantics()
	p.expectedTypeStack = append(p.expectedTypeStack, nil)
}

func (p *AdaParserBase) PopExpectedType() {
	p.initSemantics()
	if len(p.expectedTypeStack) > 0 {
		p.expectedTypeStack = p.expectedTypeStack[:len(p.expectedTypeStack)-1]
	}
}

func (p *AdaParserBase) OutputSymbolTable() {
	p.initSemantics()
	if p.outputSymbolTableFlag {
		fmt.Fprintln(os.Stderr, p.st.String())
	}
}

func (p *AdaParserBase) ParsePragmas() {
	stream, ok := p.GetTokenStream().(*antlr.CommonTokenStream)
	if !ok {
		return
	}
	stream.Fill()
	allTokens := stream.GetAllTokens()
	const PRAGMA_CHANNEL = 2
	var currentPragma []antlr.Token
	var pragmas [][]antlr.Token
	for _, token := range allTokens {
		if token.GetChannel() != PRAGMA_CHANNEL {
			continue
		}
		if token.GetTokenType() == AdaLexerPRAGMA {
			currentPragma = []antlr.Token{token}
		} else if currentPragma != nil {
			currentPragma = append(currentPragma, token)
			if token.GetTokenType() == AdaLexerSEMI {
				pragmas = append(pragmas, currentPragma)
				currentPragma = nil
			}
		}
	}
	for _, pragmaTokens := range pragmas {
		source := pragmaTokens[0].GetSource()
		var tokens []antlr.Token
		for _, t := range pragmaTokens {
			ct := antlr.NewCommonToken(source, t.GetTokenType(), antlr.TokenDefaultChannel, t.GetStart(), t.GetStop())
			ct.SetText(t.GetText())
			tokens = append(tokens, ct)
		}
		eof := antlr.NewCommonToken(source, antlr.TokenEOF, antlr.TokenDefaultChannel, -1, -1)
		tokens = append(tokens, eof)
		src := &pragmaTokenSource{
			BaseLexer: &antlr.BaseLexer{BaseRecognizer: antlr.NewBaseRecognizer()},
			toks:      tokens,
		}
		tokenStream := antlr.NewCommonTokenStream(src, antlr.TokenDefaultChannel)
		parser := NewAdaParser(tokenStream)
		parser.RemoveErrorListeners()
		parser.AddErrorListener(p.GetErrorListenerDispatch())
		parser.PragmaRule()
	}
}
