package parser

import (
	"github.com/antlr4-go/antlr/v4"
)

// JavaParserBase implementation.
type JavaParserBase struct {
	*antlr.BaseParser
}

func NewJavaParserBase(input antlr.TokenStream) *JavaParserBase {
	return new(JavaParserBase)
}

func (p *JavaParserBase) DoLastRecordComponent() bool {
	ctx := p.GetParserRuleContext()
	tctx, ok := ctx.(*RecordComponentListContext)
	if !ok {
		return true
	}
	rcs := tctx.AllRecordComponent()
	if len(rcs) == 0 {
		return true
	}
	count := len(rcs)
	for c := 0; c < count; c++ {
		if rcs[c].ELLIPSIS() != nil && c+1 < count {
			return false
		}
	}
	return true
}

func (p *JavaParserBase) IsNotIdentifierAssign() bool {
	stream := p.GetTokenStream()
	la := stream.LA(1)
	switch la {
	case JavaParserIDENTIFIER,
		JavaParserMODULE,
		JavaParserOPEN,
		JavaParserREQUIRES,
		JavaParserEXPORTS,
		JavaParserOPENS,
		JavaParserTO,
		JavaParserUSES,
		JavaParserPROVIDES,
		JavaParserWHEN,
		JavaParserWITH,
		JavaParserTRANSITIVE,
		JavaParserYIELD,
		JavaParserSEALED,
		JavaParserPERMITS,
		JavaParserRECORD,
		JavaParserVAR:
		// fall through
	default:
		return true
	}
	la2 := stream.LA(2)
	return la2 != JavaParserASSIGN
}
