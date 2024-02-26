using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;

public abstract class XPath31ParserBase : Parser
{
    protected XPath31ParserBase(ITokenStream input)
        : base(input)
    {
    }

    protected XPath31ParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }

    protected bool IsFuncCall()
    {
        return !(
            InputStream.LA(1) == XPath31Parser.KW_ARRAY
            || InputStream.LA(1) == XPath31Parser.KW_ATTRIBUTE
            || InputStream.LA(1) == XPath31Parser.KW_COMMENT
            || InputStream.LA(1) == XPath31Parser.KW_DOCUMENT_NODE
            || InputStream.LA(1) == XPath31Parser.KW_ELEMENT
            || InputStream.LA(1) == XPath31Parser.KW_EMPTY_SEQUENCE
            || InputStream.LA(1) == XPath31Parser.KW_FUNCTION
            || InputStream.LA(1) == XPath31Parser.KW_IF
            || InputStream.LA(1) == XPath31Parser.KW_ITEM
            || InputStream.LA(1) == XPath31Parser.KW_MAP
            || InputStream.LA(1) == XPath31Parser.KW_NAMESPACE_NODE
            || InputStream.LA(1) == XPath31Parser.KW_NODE
            || InputStream.LA(1) == XPath31Parser.KW_PROCESSING_INSTRUCTION
            || InputStream.LA(1) == XPath31Parser.KW_SCHEMA_ATTRIBUTE
            || InputStream.LA(1) == XPath31Parser.KW_SCHEMA_ELEMENT
            || InputStream.LA(1) == XPath31Parser.KW_TEXT
        );
    }
}
