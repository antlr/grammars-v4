import org.antlr.v4.runtime.*;

public abstract class XPath20ParserBase extends Parser
{
    protected XPath20ParserBase(TokenStream input)
    {
        super(input);
    }

    protected boolean IsFuncCall()
    {
		return !(
		    getInputStream().LA(1) == XPath20Parser.KW_ARRAY
		    || getInputStream().LA(1) == XPath20Parser.KW_ATTRIBUTE
			|| getInputStream().LA(1) == XPath20Parser.KW_COMMENT
			|| getInputStream().LA(1) == XPath20Parser.KW_DOCUMENT_NODE
			|| getInputStream().LA(1) == XPath20Parser.KW_ELEMENT
			|| getInputStream().LA(1) == XPath20Parser.KW_EMPTY_SEQUENCE
			|| getInputStream().LA(1) == XPath20Parser.KW_FUNCTION
			|| getInputStream().LA(1) == XPath20Parser.KW_IF
			|| getInputStream().LA(1) == XPath20Parser.KW_ITEM
			|| getInputStream().LA(1) == XPath20Parser.KW_MAP
			|| getInputStream().LA(1) == XPath20Parser.KW_NAMESPACE_NODE
			|| getInputStream().LA(1) == XPath20Parser.KW_NODE
			|| getInputStream().LA(1) == XPath20Parser.KW_PROCESSING_INSTRUCTION
			|| getInputStream().LA(1) == XPath20Parser.KW_SCHEMA_ATTRIBUTE
			|| getInputStream().LA(1) == XPath20Parser.KW_SCHEMA_ELEMENT
			|| getInputStream().LA(1) == XPath20Parser.KW_TEXT
		);
    }
}
