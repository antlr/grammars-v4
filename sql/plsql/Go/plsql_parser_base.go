package parser

import (
    "github.com/antlr4-go/antlr/v4"
)

// PlSqlParserBase implementation.
type PlSqlParserBase struct {
    *antlr.BaseParser
    _isVersion12 bool
    _isVersion11 bool
    _isVersion10 bool
    _lastUnitWasPlsql bool
}

var StaticConfig PlSqlParserBase

func init() {
    StaticConfig = PlSqlParserBase {
        _isVersion12: true,
        _isVersion11: true,
	    _isVersion10: true,
    }
}

func (p *PlSqlParserBase) reset() {
    p._lastUnitWasPlsql = false
    p.BaseParser.Reset()
}

func (p *PlSqlParserBase) setLastUnitPlsql() { p._lastUnitWasPlsql = true }
func (p *PlSqlParserBase) setLastUnitSql()   { p._lastUnitWasPlsql = false }
func (p *PlSqlParserBase) isLastUnitSql() bool    { return !p._lastUnitWasPlsql }
func (p *PlSqlParserBase) isLastUnitPlsql() bool  { return p._lastUnitWasPlsql }

func (p *PlSqlParserBase) isSolidusSeparator() bool {
    stream := p.GetTokenStream().(*antlr.CommonTokenStream)
    solidus := stream.LT(1)
    if solidus == nil || solidus.GetTokenType() != PlSqlParserSOLIDUS {
        return false
    }

    solidusLine := solidus.GetLine()

    prev := stream.LT(-1)
    if prev != nil && prev.GetTokenType() != antlr.TokenEOF && prev.GetLine() == solidusLine {
        return false
    }

    next := stream.LT(2)
    if next != nil && next.GetTokenType() != antlr.TokenEOF && next.GetLine() == solidusLine {
        return false
    }

    return true
}

func (p *PlSqlParserBase) isVersion12() bool {
    return StaticConfig._isVersion12;
}

func (p *PlSqlParserBase) setVersion12(value bool) {
    StaticConfig._isVersion12 = value;
}

func (p *PlSqlParserBase) isVersion11() bool {
    return StaticConfig._isVersion11;
}

func (p *PlSqlParserBase) setVersion11(value bool) {
    StaticConfig._isVersion11 = value;
}

func (p *PlSqlParserBase) isVersion10() bool {
    return StaticConfig._isVersion10;
}

func (p *PlSqlParserBase) setVersion10(value bool) {
    StaticConfig._isVersion10 = value;
}

func (p *PlSqlParserBase) IsNotNumericFunction() bool {
    stream := p.GetTokenStream().(*antlr.CommonTokenStream)
    lt1 := stream.LT(1)
    lt2 := stream.LT(2)
    if (lt1.GetTokenType() == PlSqlParserSUM ||
        lt1.GetTokenType() == PlSqlParserCOUNT ||
        lt1.GetTokenType() == PlSqlParserAVG ||
        lt1.GetTokenType() == PlSqlParserMIN ||
        lt1.GetTokenType() == PlSqlParserMAX ||
        lt1.GetTokenType() == PlSqlParserROUND ||
        lt1.GetTokenType() == PlSqlParserLEAST ||
        lt1.GetTokenType() == PlSqlParserGREATEST) &&
        lt2.GetTokenType() == PlSqlParserLEFT_PAREN {
        return false
    }
    return true
}

func (p *PlSqlParserBase) isNotStartOfJoin() bool {
    stream := p.GetTokenStream().(*antlr.CommonTokenStream)
    lt1 := stream.LT(1)
    if lt1.GetTokenType() == PlSqlParserINNER ||
       lt1.GetTokenType() == PlSqlParserCROSS ||
       lt1.GetTokenType() == PlSqlParserNATURAL ||
       lt1.GetTokenType() == PlSqlParserPARTITION ||
       lt1.GetTokenType() == PlSqlParserFULL ||
       lt1.GetTokenType() == PlSqlParserLEFT ||
       lt1.GetTokenType() == PlSqlParserRIGHT ||
       lt1.GetTokenType() == PlSqlParserOUTER {
        return false
    }
    return true
}
