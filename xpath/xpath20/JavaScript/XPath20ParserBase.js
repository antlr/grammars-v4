import antlr4 from 'antlr4';
import XPath20Parser from './XPath20Parser.js';

export default class XPath20ParserBase extends antlr4.Parser {

    constructor(input) {
        super(input);
    }

    IsFuncCall() {
        const nextTokenType = this._input.LT(1).type;
        return !(
		 nextTokenType == this.KW_ARRAY
		 || nextTokenType == this.KW_ATTRIBUTE
		 || nextTokenType == this.KW_COMMENT
		 || nextTokenType == this.KW_DOCUMENT_NODE
		 || nextTokenType == this.KW_ELEMENT
		 || nextTokenType == this.KW_EMPTY_SEQUENCE
		 || nextTokenType == this.KW_FUNCTION
		 || nextTokenType == this.KW_IF
		 || nextTokenType == this.KW_ITEM
		 || nextTokenType == this.KW_MAP
		 || nextTokenType == this.KW_NAMESPACE_NODE
		 || nextTokenType == this.KW_NODE
		 || nextTokenType == this.KW_PROCESSING_INSTRUCTION
		 || nextTokenType == this.KW_SCHEMA_ATTRIBUTE
		 || nextTokenType == this.KW_SCHEMA_ELEMENT
		 || nextTokenType == this.KW_TEXT
               );
    }
}
