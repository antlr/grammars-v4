package parser

import (
        "github.com/antlr4-go/antlr/v4"
        "fmt"
        "os"
        "strings"
)

// GoParserBase implementation.
type GoParserBase struct {
        *antlr.BaseParser
        debug   bool
        table   map[string]bool
}

func hasArg(args []string, arg string) bool {
    argLower := strings.ToLower(arg)
    for _, a := range args {
        if strings.Contains(strings.ToLower(a), argLower) {
            return true
        }
    }
    return false
}

func NewGoParserBase(input antlr.TokenStream) *GoParserBase {
    p := &GoParserBase{
        table: make(map[string]bool),
    }
    p.debug = hasArg(os.Args, "--debug")
    if p.debug {
        fmt.Println("debug =", p.debug)
    }
    return p
}

func (p *GoParserBase) myreset() {
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
    // Initialize table if nil (can happen if myreset() wasn't called)
    if p.table == nil {
        p.table = make(map[string]bool)
    }
    ctx := p.GetParserRuleContext()
    importSpec, ok := ctx.(IImportSpecContext)
    if !ok || importSpec == nil {
        return
    }
    packageName := importSpec.PackageName()
    if packageName != nil {
        name := packageName.GetText()
        if p.debug {
            fmt.Println("Entering " + name)
        }
        p.table[name] = true
        return
    }
    importPath := importSpec.ImportPath()
    if importPath == nil {
        return
    }
    name := importPath.GetText()
    if p.debug {
        fmt.Println("import path " + name)
    }
    name = strings.ReplaceAll(name, "\"", "")
    if len(name) == 0 {
        return
    }
    name = strings.ReplaceAll(name, "\\", "/")
    pathArr := strings.Split(name, "/")
    if len(pathArr) == 0 {
        return
    }
    lastComponent := pathArr[len(pathArr)-1]
    if len(lastComponent) == 0 {
        return
    }
    // Handle special cases like "." and ".."
    if lastComponent == "." || lastComponent == ".." {
        return
    }
    fileArr := strings.Split(lastComponent, ".")
    // Guard against empty array (can happen if lastComponent is all dots)
    if len(fileArr) == 0 {
        p.table[lastComponent] = true
        if p.debug {
            fmt.Println("Entering " + lastComponent)
        }
        return
    }
    fileName := fileArr[len(fileArr)-1]
    if len(fileName) == 0 {
        // Fall back to lastComponent if split resulted in empty string
        fileName = lastComponent
    }
    if p.debug {
        fmt.Println("Entering " + fileName)
    }
    p.table[fileName] = true
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
