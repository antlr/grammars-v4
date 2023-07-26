package parser

import (
	"github.com/antlr4-go/antlr/v4"
	"reflect"
)

// ParserBase implementation.
type CPP14ParserBase struct {
	*antlr.BaseParser
}


// Returns true if the current Token is a closing bracket (")" or "}")
func (p *CPP14ParserBase) IsPureSpecifierAllowed() bool {
    x := p.GetParserRuleContext();
    y := x.GetChild(0)
    c := y.GetChild(0)
    c2 := c.GetChild(0)
    if c2.GetChildCount() <= 1 {
	return false
    }
    c3 := c2.GetChild(1)
    if c3 == nil {
        return false
    }
    ce1 := reflect.TypeOf(c3).Elem()
    ce2 := reflect.TypeOf(ParametersAndQualifiersContext{})
    ce := ce1 == ce2
    return ce
}
