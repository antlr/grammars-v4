using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;

public abstract class XQuery4ParserBase : Parser
{
    protected XQuery4ParserBase(ITokenStream input)
        : base(input)
    {
    }

    protected XQuery4ParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }

    // Returns true when the current token can start a function call.
    // Excludes keywords that introduce other constructs (node tests, type tests, etc.)
    // so they are not mistakenly parsed as function calls.
    protected bool IsFuncCall()
    {
        return !(
            InputStream.LA(1) == XQuery4Parser.KW_ARRAY
            || InputStream.LA(1) == XQuery4Parser.KW_ATTRIBUTE
            || InputStream.LA(1) == XQuery4Parser.KW_COMMENT
            || InputStream.LA(1) == XQuery4Parser.KW_DOCUMENT_NODE
            || InputStream.LA(1) == XQuery4Parser.KW_ELEMENT
            || InputStream.LA(1) == XQuery4Parser.KW_EMPTY_SEQUENCE
            || InputStream.LA(1) == XQuery4Parser.KW_FN
            || InputStream.LA(1) == XQuery4Parser.KW_FUNCTION
            || InputStream.LA(1) == XQuery4Parser.KW_GNODE
            || InputStream.LA(1) == XQuery4Parser.KW_IF
            || InputStream.LA(1) == XQuery4Parser.KW_ITEM
            || InputStream.LA(1) == XQuery4Parser.KW_JNODE
            || InputStream.LA(1) == XQuery4Parser.KW_MAP
            || InputStream.LA(1) == XQuery4Parser.KW_NAMESPACE_NODE
            || InputStream.LA(1) == XQuery4Parser.KW_NODE
            || InputStream.LA(1) == XQuery4Parser.KW_PROCESSING_INSTRUCTION
            || InputStream.LA(1) == XQuery4Parser.KW_RECORD
            || InputStream.LA(1) == XQuery4Parser.KW_SCHEMA_ATTRIBUTE
            || InputStream.LA(1) == XQuery4Parser.KW_SCHEMA_ELEMENT
            || InputStream.LA(1) == XQuery4Parser.KW_SWITCH
            || InputStream.LA(1) == XQuery4Parser.KW_TEXT
            || InputStream.LA(1) == XQuery4Parser.KW_TYPESWITCH
        );
    }
}
