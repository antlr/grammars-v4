package parser

import (
	"github.com/antlr4-go/antlr/v4"
//	"reflect"
//	"fmt"
)

// ParserBase implementation.
type CPP14ParserBase struct {
	*antlr.BaseParser
}


// Returns true if the current Token is a closing bracket (")" or "}")
func (p *CPP14ParserBase) IsPureSpecifierAllowed() bool {
//    fmt.Println("YO!")
    x := p.GetParserRuleContext();
//    fmt.Println("YO 1", x)
    y := x.GetChild(0)
//    fmt.Println("YO 2", y)
    c := y.GetChild(0)
//    fmt.Println("YO 3 ", c)
    c2 := c.GetChild(0)
//    fmt.Println("YO 4 ", c2)
    if c2.GetChildCount() <= 1 {
//	fmt.Println("return false")
	return false
    }
//    c3 := c2.GetChild(1)
//    if c3 == nil {
//        return false
//    }
//    return reflect.TypeOf(c3).Elem() == reflect.TypeOf(ParametersAndQualifiersContext{}).Elem()
//    fmt.Println("return true")
    return true
}
