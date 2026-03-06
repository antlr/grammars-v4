package parser

import (
	"os"
	"strings"

	"github.com/antlr4-go/antlr/v4"
)

var allSemanticFunctions = []string{"IsLocalVariableDeclaration"}

type CSharpParserBase struct {
	*antlr.BaseParser
	noSemantics map[string]bool
	initialized bool
}

func (p *CSharpParserBase) initSemantics() {
	if p.initialized {
		return
	}
	p.initialized = true
	p.noSemantics = make(map[string]bool)
	for _, arg := range os.Args {
		lower := strings.ToLower(arg)
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

func (p *CSharpParserBase) IsLocalVariableDeclaration() bool {
	p.initSemantics()
	if p.noSemantics["IsLocalVariableDeclaration"] {
		return true
	}
	ctx := p.GetParserRuleContext()
	local_var_decl, ok := ctx.(*Local_variable_declarationContext)
	if !ok || local_var_decl == nil {
		return true
	}
	local_variable_type := local_var_decl.Local_variable_type()
	if local_variable_type == nil {
		return true
	}
	if local_variable_type.GetText() == "var" {
		return false
	}
	return true
}
