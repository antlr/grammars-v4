package parser

import (
        "github.com/antlr4-go/antlr/v4"
        "fmt"
        "strings"
)

// GoParserBase implementation.
type GoParserBase struct {
        *antlr.BaseParser
        debug   bool
        table   map[string]bool
}

func (p *GoParserBase) myreset() {
    p.debug = false
    p.table = make(map[string]bool)
}

func (p *GoParserBase) closingBracket() bool {
    stream := p.GetTokenStream()
    la := stream.LT(1)
    return la.GetTokenType() == GoParserR_PAREN || la.GetTokenType() == GoParserR_CURLY || la.GetTokenType() == antlr.TokenEOF;
}

func (p *GoParserBase) isNotReceive() bool {
    stream := p.GetTokenStream()
    la := stream.LT(2)
    return la.GetTokenType() != GoParserRECEIVE;
}

func (p *GoParserBase) addImportSpec() {
    ctx := p.GetParserRuleContext()
    importSpec := ctx.(IImportSpecContext)
    if importSpec == nil {
        return;
    }
    packageName := importSpec.PackageName()
    if packageName != nil {
        name := packageName.GetText()
        if p.debug {
            fmt.Println("Entering " + name)
            p.table[name] = true
        }
    } else {
        name := importSpec.ImportPath().GetText()
        name = strings.ReplaceAll(name, "\"", "")
        name = strings.ReplaceAll(name, "\\", "/")
        pathArr := strings.Split(name, "/")
        fileName := pathArr[len(pathArr)-1]
        if p.debug {
            fmt.Println("Entering " + fileName)
        }
        p.table[fileName] = true
    }
}

func (p *GoParserBase) isOperand() bool {
    stream := p.GetTokenStream()
    la := stream.LT(1)
    result := true
    if la.GetText() == "err" {
        return true
    }
    if la.GetTokenType() != GoParserIDENTIFIER {
        if p.debug {
            fmt.Println("isOperand Returning ", result, " for ", la)
        }
        return result
    }
    result, _ = p.table[la.GetText()]
    la2 := stream.LT(2)
    if la2.GetTokenType() != GoParserDOT {
        result = true
        if p.debug {
            fmt.Println("isOperand Returning ", result, " for ", la)
        }
        return result
    }
    la3 := stream.LT(3)
    if la3.GetTokenType() == GoParserL_PAREN {
        result = true
        if p.debug {
            fmt.Println("isOperand Returning ", result, " for ", la)
        }
        return result
    }
    if p.debug {
        fmt.Println("isOperand Returning ", result, " for ", la)
    }
    return result
}

func (p *GoParserBase) isConversion() bool {
    stream := p.GetTokenStream()
    la := stream.LT(1)
    result := la.GetTokenType() != GoParserIDENTIFIER;
    if p.debug {
        fmt.Println("isConversion Returning ", result, " for ", la)
    }
    return result
}

func (p *GoParserBase) isMethodExpr() bool {
    stream := p.GetTokenStream()
    la := stream.LT(1)
    result := true
    if la.GetTokenType() == GoParserSTAR {
        if p.debug {
            fmt.Println("isMethodExpr Returning ", result, " for ", la)
        }
        return result
    }
    if la.GetTokenType() != GoParserIDENTIFIER {
        result = false
        if p.debug {
            fmt.Println("isMethodExpr Returning ", result, " for ", la)
        }
        return result
    }
    _, found := p.table[la.GetText()]
    if ! found {
        result = true
    }
    if p.debug {
        fmt.Println("isMethodExpr Returning ", result, " for ", la)
    }
    return result
}
