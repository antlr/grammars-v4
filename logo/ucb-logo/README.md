# Latte Logo

An ANTLR 4 grammar for [UCB Logo](http://www.cs.berkeley.edu/~bh/usermanual).

The grammar including regression tests can be found 
[here](https://github.com/bkiers/logo-parser).

The grammar has target specific code embedded in it because both tokenization
and parsing is context sensitive.

## Lexer

Take the following Logo source:

```
print 1+2
```

which will result in `3` being printed. However, when `1+2` is inside a list:

```
print [1+2]
```

then `1+2` would be printed. Note that the list would hold just a single
word token, not 3 separate word tokens. The same applies to arrays:

```
;; the array would also hold a single word value containing "1+2"
print {1+2}
```

## Parser

There are no strict delimiters that act as the end or procedure calls in Logo.
The following two snippets are therefor valid Logo sources:

```
print "a print "b
```

and

```
print "a
print "b
```

which makes parsing user defined procedures problematic. For example, the user
defined procedures `p1` and `p2` and invokes them as follows:

```
print p1 p2 10 20
```

without knowing how many parameters each procedure expects, it is impossible to
parse. All of the following parses are possible:

```
(print (p1 (p2 10 20))) ;; p1 one param, p2 two params
(print (p1 (p2 10) 20)) ;; p1 two params, p2 one param
(print (p1 (p2) 10 20)) ;; p1 three params, p2 no params
```

To resolve this, the parser has a `Map<String, Integer>` that is initialised with
all built-in Logo procedures and macros and the number of parameters each expects.

When the parser is instantiated and the `parse` rule is invoked, it makes an
initial pass over the input collecting all user defined procedures and macros:

```Java
/**
 * Creates a new instance of a {@code UCBLogoParser} where
 * any user defined procedures will be resolved in an initial
 * parse.
 *
 * @param input
 *         the inout stream containing the UCB Logo source
 *         to parse.
 */
public UCBLogoParser(ANTLRInputStream input) {
  this(new CommonTokenStream(new UCBLogoLexer(input)));

  // Create a lexer and parser that will resolve user defined procedures.
  UCBLogoLexer lexer = new UCBLogoLexer(input);
  UCBLogoParser parser = new UCBLogoParser(new CommonTokenStream(lexer));

  ParseTreeWalker.DEFAULT.walk(new UCBLogoBaseListener(){

    @Override
    public void enterProcedure_def(@NotNull UCBLogoParser.Procedure_defContext ctx) {
      // Yes, we found a procedure: save it in the procedures-map.
      procedures.put(ctx.NAME().getText(), ctx.variables.amount);
    }

    // The same for macro-defs

  }, parser.parse());

  // Reset the input stream after having resolved the user defined procedures.
  input.reset();

  this.discoveredAllProcedures = true;
}
```

On the first pass, the body of a procedure- or macro-definition is parsed
as a flat list of tokens. And on the second pass, we properly parse the body
as denoted by the rule `body_def`:

```ANTLR
body_def
    : {discoveredAllProcedures}? body_instruction* END // second pass
    |                            ~END* END             // fist pass
    ;
```

This is done because the body of a procedure or macro could contain a call
to a procedure defined later.

Take the following Logo source for example:

```
;; One param: divides a number in two.
to p1 :n
  output :n / 2
end

;; Two params: adds 2 numbers.
to p2 :n1 :n2
  output :n1 + :n2
end

print p1 p2 10 20
```

The first pass of the parser the `body_def` will just contain a flat list
of tokens:

```
'- parse
   |- procedureDefInstruction
   |  '- procedure_def
   |     |- to
   |     |- p1
   |     |- variables
   |     |  '- :n
   |     '- body_def
   |        |- output
   |        |- :n
   |        |- /
   |        |- 2
   |        '- end
   |- procedureDefInstruction
   |  '- procedure_def
   |     |- to
   |     |- p2
   |     |- variables
   |     |  |- :n1
   |     |  '- :n2
   |     '- body_def
   |        |- output
   |        |- :n1
   |        |- +
   |        |- :n2
   |        '- end
   |- procedureCallInstruction
   |  '- procedure_call
   |     |- print
   |     '- expressions
   |        '- procedureCallExpression
   |           '- procedure_call
   |              |- p1
   |              '- expressions
   |                 '- procedureCallExpression
   |                    '- procedure_call
   |                       |- p2
   |                       '- expressions
   |                          |- numberExpression
   |                          |  '- 10
   |                          '- numberExpression
   |                             '- 20
   '- <EOF>
```

And after the second pass, the input is parsed as expected: `p1` taking
one parameter, and `p2` taking two parameters:

```
'- parse
   |- procedureDefInstruction
   |  '- procedure_def
   |     |- to
   |     |- p1
   |     |- variables
   |     |  '- :n
   |     '- body_def
   |        |- body_instruction
   |        |  '- procedure_call
   |        |     |- output
   |        |     '- expressions
   |        |        '- divideExpression
   |        |           |- variableExpression
   |        |           |  '- :n
   |        |           |- /
   |        |           '- numberExpression
   |        |              '- 2
   |        '- end
   |- procedureDefInstruction
   |  '- procedure_def
   |     |- to
   |     |- p2
   |     |- variables
   |     |  |- :n1
   |     |  '- :n2
   |     '- body_def
   |        |- body_instruction
   |        |  '- procedure_call
   |        |     |- output
   |        |     '- expressions
   |        |        '- additionExpression
   |        |           |- variableExpression
   |        |           |  '- :n1
   |        |           |- +
   |        |           '- variableExpression
   |        |              '- :n2
   |        '- end
   |- procedureCallInstruction
   |  '- procedure_call
   |     |- print
   |     '- expressions
   |        '- procedureCallExpression
   |           '- procedure_call
   |              |- p1
   |              '- expressions
   |                 '- procedureCallExpression
   |                    '- procedure_call
   |                       |- p2
   |                       '- expressions
   |                          |- numberExpression
   |                          |  '- 10
   |                          '- numberExpression
   |                             '- 20
   '- <EOF>
```