# Positive Technologies Java8 grammar

Based on the previous optimized Java7 grammar by Terence Parr and Sam Harwell
with the same BSD license.

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

The grammar contains `AllInOne8.Java` file that almost fully covered Java syntax.
