# Cypher grammar

An ANTLR4 grammar for the [Cypher](http://opencypher.org/) language

## Version Selection

The lexer and parser support selecting the Cypher standard so the same grammar can parse multiple dialects.

- Supported standards: `CYPHER3`, `CYPHER4`, `CYPHER5`
- Default: `CYPHER5`
- Members are defined in the grammar files
  - Lexer members: `cypher/CypherLexer.g4:41`
  - Parser members: `cypher/CypherParser.g4:32`

### Usage (Java target)

```java
CypherLexer lexer = new CypherLexer(input);
lexer.setStandard(CypherLexer.Standard.CYPHER5); // CYPHER3, CYPHER4, CYPHER5

CommonTokenStream tokens = new CommonTokenStream(lexer);
CypherParser parser = new CypherParser(tokens);
parser.setStandard(CypherParser.Standard.CYPHER5);

CypherParser.ScriptContext cu = parser.script();
```

### Gated Features

- Cypher 4+
  - `EXISTS`: `cypher/CypherLexer.g4:94`
- Cypher 5+
  - `REQUIRE`, `FOR`, `DO`, `UNIQUE`, `MANDATORY`, `SCALAR`, `OF`, `ADD`, `DROP`: `cypher/CypherLexer.g4:116–129`

When a feature is gated off for an earlier standard, its keyword lexes as an identifier and grammar constructs using that keyword will not match.

### Multiple Statements

- The `script` rule accepts multiple queries separated by semicolons.
  - `cypher/CypherParser.g4:38–40`