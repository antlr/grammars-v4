import { Parser, TokenStream, BufferedTokenStream } from 'antlr4';
import JavaParser, { RecordComponentContext, RecordComponentListContext } from './JavaParser.js';

export default abstract class JavaParserBase extends Parser {

    constructor(input: TokenStream) {
        super(input);
    }

    public DoLastRecordComponent(): boolean {
        const ctx = this._ctx;
        if (!(ctx instanceof RecordComponentListContext)) return true;
        const rcs = (ctx.children ?? []).filter(c => c instanceof RecordComponentContext) as RecordComponentContext[];
        if (rcs.length === 0) return true;
        const count = rcs.length;
        for (let c = 0; c < count; ++c) {
            if (rcs[c].ELLIPSIS() !== null && c + 1 < count) return false;
        }
        return true;
    }

    public IsNotIdentifierAssign(): boolean {
        const stream = this._input as BufferedTokenStream;
        const la = stream.LA(1);
        switch (la) {
            case JavaParser.IDENTIFIER:
            case JavaParser.MODULE:
            case JavaParser.OPEN:
            case JavaParser.REQUIRES:
            case JavaParser.EXPORTS:
            case JavaParser.OPENS:
            case JavaParser.TO:
            case JavaParser.USES:
            case JavaParser.PROVIDES:
            case JavaParser.WHEN:
            case JavaParser.WITH:
            case JavaParser.TRANSITIVE:
            case JavaParser.YIELD:
            case JavaParser.SEALED:
            case JavaParser.PERMITS:
            case JavaParser.RECORD:
            case JavaParser.VAR:
                break;
            default:
                return true;
        }
        const la2 = stream.LA(2);
        if (la2 !== JavaParser.ASSIGN) return true;
        return false;
    }
}
