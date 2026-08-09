for $function in //functionCall
    [./functionName/qname => string() = ('matches', 'replace', 'tokenize', 'analyze-string',
                                          'fn:matches', 'fn:replace', 'fn:tokenize', 'fn:analyze-string') ]
return if ($pattern-string => exists()) then $pattern-string else ()
