import antlr4 from 'antlr4';
import XQuery4Parser from './XQuery4Parser.js';

export default class XQuery4ParserBase extends antlr4.Parser {

    constructor(input) {
        super(input);
    }

    // Returns true when the current token can start a function call.
    IsFuncCall() {
        const t = this._input.LT(1).type;
        return !(
            t === XQuery4Parser.KW_ARRAY
            || t === XQuery4Parser.KW_ATTRIBUTE
            || t === XQuery4Parser.KW_COMMENT
            || t === XQuery4Parser.KW_DOCUMENT_NODE
            || t === XQuery4Parser.KW_ELEMENT
            || t === XQuery4Parser.KW_EMPTY_SEQUENCE
            || t === XQuery4Parser.KW_FN
            || t === XQuery4Parser.KW_FUNCTION
            || t === XQuery4Parser.KW_GNODE
            || t === XQuery4Parser.KW_IF
            || t === XQuery4Parser.KW_ITEM
            || t === XQuery4Parser.KW_JNODE
            || t === XQuery4Parser.KW_MAP
            || t === XQuery4Parser.KW_NAMESPACE_NODE
            || t === XQuery4Parser.KW_NODE
            || t === XQuery4Parser.KW_PROCESSING_INSTRUCTION
            || t === XQuery4Parser.KW_RECORD
            || t === XQuery4Parser.KW_SCHEMA_ATTRIBUTE
            || t === XQuery4Parser.KW_SCHEMA_ELEMENT
            || t === XQuery4Parser.KW_SWITCH
            || t === XQuery4Parser.KW_TEXT
            || t === XQuery4Parser.KW_TYPESWITCH
        );
    }
}
