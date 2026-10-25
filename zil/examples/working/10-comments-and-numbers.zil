;"A comment before a decimal, hexadecimal, and binary value."
<CONSTANT DECIMAL 42>
<CONSTANT HEX *2A*>
<CONSTANT BINARY #2101010>
<OBJECT COMMENT-EXAMPLE (DESC "visible" ;"ignored")>
;<GLOBAL COMMENTED-OUT
    <TABLE (PURE) 1 2>>
<ROUTINE COMMENT-TEST ()
    <TELL ;IGNORED "kept">
    ;(<TELL "ignored">)
    <RTRUE>>
