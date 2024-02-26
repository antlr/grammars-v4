using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;

public abstract class XPath20ParserBase : Parser
{
    protected XPath20ParserBase(ITokenStream input)
        : base(input)
    {
    }

    protected XPath20ParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }

    protected bool IsFuncCall()
    {
        return !(
            InputStream.LA(1) == XPath20Parser.KW_ARRAY
            || InputStream.LA(1) == XPath20Parser.KW_ATTRIBUTE
            || InputStream.LA(1) == XPath20Parser.KW_COMMENT
            || InputStream.LA(1) == XPath20Parser.KW_DOCUMENT_NODE
            || InputStream.LA(1) == XPath20Parser.KW_ELEMENT
            || InputStream.LA(1) == XPath20Parser.KW_EMPTY_SEQUENCE
            || InputStream.LA(1) == XPath20Parser.KW_FUNCTION
            || InputStream.LA(1) == XPath20Parser.KW_IF
            || InputStream.LA(1) == XPath20Parser.KW_ITEM
            || InputStream.LA(1) == XPath20Parser.KW_MAP
            || InputStream.LA(1) == XPath20Parser.KW_NAMESPACE_NODE
            || InputStream.LA(1) == XPath20Parser.KW_NODE
            || InputStream.LA(1) == XPath20Parser.KW_PROCESSING_INSTRUCTION
            || InputStream.LA(1) == XPath20Parser.KW_SCHEMA_ATTRIBUTE
            || InputStream.LA(1) == XPath20Parser.KW_SCHEMA_ELEMENT
            || InputStream.LA(1) == XPath20Parser.KW_TEXT
        );
    }
}
