import org.antlr.v4.runtime.*;

public abstract class XPath31ParserBase extends Parser
{
    protected XPath31ParserBase(TokenStream input)
    {
        super(input);
    }

    protected boolean IsFuncCall()
    {
		return !(
		    getInputStream().LA(1) == XPath31Parser.KW_ARRAY
		    || getInputStream().LA(1) == XPath31Parser.KW_ATTRIBUTE
			|| getInputStream().LA(1) == XPath31Parser.KW_COMMENT
			|| getInputStream().LA(1) == XPath31Parser.KW_DOCUMENT_NODE
			|| getInputStream().LA(1) == XPath31Parser.KW_ELEMENT
			|| getInputStream().LA(1) == XPath31Parser.KW_EMPTY_SEQUENCE
			|| getInputStream().LA(1) == XPath31Parser.KW_FUNCTION
			|| getInputStream().LA(1) == XPath31Parser.KW_IF
			|| getInputStream().LA(1) == XPath31Parser.KW_ITEM
			|| getInputStream().LA(1) == XPath31Parser.KW_MAP
			|| getInputStream().LA(1) == XPath31Parser.KW_NAMESPACE_NODE
			|| getInputStream().LA(1) == XPath31Parser.KW_NODE
			|| getInputStream().LA(1) == XPath31Parser.KW_PROCESSING_INSTRUCTION
			|| getInputStream().LA(1) == XPath31Parser.KW_SCHEMA_ATTRIBUTE
			|| getInputStream().LA(1) == XPath31Parser.KW_SCHEMA_ELEMENT
			|| getInputStream().LA(1) == XPath31Parser.KW_TEXT
		);
    }
}
