import org.antlr.v4.runtime.*;

public abstract class XQuery4ParserBase extends Parser
{
    protected XQuery4ParserBase(TokenStream input)
    {
        super(input);
    }

    // Returns true when the current token can start a function call.
    // Excludes keywords that introduce other constructs so they are not
    // mistakenly parsed as function calls.
    protected boolean IsFuncCall()
    {
        return !(
            getInputStream().LA(1) == XQuery4Parser.KW_ARRAY
            || getInputStream().LA(1) == XQuery4Parser.KW_ATTRIBUTE
            || getInputStream().LA(1) == XQuery4Parser.KW_COMMENT
            || getInputStream().LA(1) == XQuery4Parser.KW_DOCUMENT_NODE
            || getInputStream().LA(1) == XQuery4Parser.KW_ELEMENT
            || getInputStream().LA(1) == XQuery4Parser.KW_EMPTY_SEQUENCE
            || getInputStream().LA(1) == XQuery4Parser.KW_FN
            || getInputStream().LA(1) == XQuery4Parser.KW_FUNCTION
            || getInputStream().LA(1) == XQuery4Parser.KW_GNODE
            || getInputStream().LA(1) == XQuery4Parser.KW_IF
            || getInputStream().LA(1) == XQuery4Parser.KW_ITEM
            || getInputStream().LA(1) == XQuery4Parser.KW_JNODE
            || getInputStream().LA(1) == XQuery4Parser.KW_MAP
            || getInputStream().LA(1) == XQuery4Parser.KW_NAMESPACE_NODE
            || getInputStream().LA(1) == XQuery4Parser.KW_NODE
            || getInputStream().LA(1) == XQuery4Parser.KW_PROCESSING_INSTRUCTION
            || getInputStream().LA(1) == XQuery4Parser.KW_RECORD
            || getInputStream().LA(1) == XQuery4Parser.KW_SCHEMA_ATTRIBUTE
            || getInputStream().LA(1) == XQuery4Parser.KW_SCHEMA_ELEMENT
            || getInputStream().LA(1) == XQuery4Parser.KW_SWITCH
            || getInputStream().LA(1) == XQuery4Parser.KW_TEXT
            || getInputStream().LA(1) == XQuery4Parser.KW_TYPESWITCH
        );
    }
}
