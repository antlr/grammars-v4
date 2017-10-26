# Java grammar by [Positive Technologies](https://github.com/PositiveTechnologies)

Based on the previous optimized [Java7](../java) grammar by Terence Parr and Sam Harwell
with the same BSD license. This grammar does not exactly corresponds to the formal
Java specification unlike usual [Java8](../java8) grammar, but passes tests such as
[AllInOne7.Java](examples/AllInOne7.java) and [AllInOne8.java](examples/AllInOne8.java).
Performance, practical usage and clarity in priority.

This grammar parses the file [ManyStringsConcat.java](examples/ManyStringsConcat.java)
much more faster than original grammar without left recursion expressions.

## Supported Java versions

* Java 7
* Java 8

## Grammar style

### Parse rules

```ANTLR
parserRule
    : token1 (token2* OPERATOR token3?)
    ;
```

### Tokens

```ANTLR
INT:                'int';
INTERFACE:          'interface';
```

### Fragments

```ANTLR
fragment
HexDigit
    : [0-9a-fA-F]
    ;
```

### Tokens using

Please use token names instead of literal names if possible and justified.
It's more convenient during parse tree bypass.

```ANTLR
modifier
    : classOrInterfaceModifier
    | NATIVE
    | SYNCHRONIZED
    | TRANSIENT
    | VOLATILE
    ;
```

instead of

```ANTLR
modifier
    : classOrInterfaceModifier
    | 'native'
    | 'synchronized'
    | 'transient'
    | 'volatile'
    ;
```

## Tests

The grammar contains [AllInOne7.java](examples/AllInOne7.java) and
[AllInOne8.java](examples/AllInOne8.java) files that almost fully covered Java syntax.
