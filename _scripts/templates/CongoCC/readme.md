# CongoCC target

`trgen -t CongoCC` generates a Java parser application. `build.sh` uses the
Trash pipeline to convert an ANTLR4 grammar with `convert.xq`, runs
CongoCC, and compiles the generated Java code.
For separate parser and lexer grammars, the build first runs `dotnet trash combine`
on their parse-result bundle. The converter requires action-free grammar rules.

Set `CONGOCC_JAR` to the CongoCC generator JAR. Its default is
`$HOME/congo-parser-generator/bin/congocc.jar`. Java and `javac` must be on
`PATH` (or set `JAVA` and `JAVAC`).

Use `bash run.sh -tree FILE` to print a parenthesized ANTLR
`ToStringTree()`-style parse tree to stderr. `--no-output` suppresses the timing
line while retaining the tree. The generated grammar sets
`SMART_NODE_CREATION=false` so single-child parser rules remain visible.
Direct left-recursion lowering may still produce a different tree shape from
an ANTLR parser.

After building, run `make test` (or `bash test.sh`) to test the input patterns
from the source grammar's `desc.xml`. `pwsh test.ps1` provides the same check
on Windows. A nonempty sibling `.errors` file marks an expected rejection;
when a sibling `.tree` file exists, the test compares it with the CongoCC
`-tree` output. Error messages are not compared because CongoCC reports them
differently from ANTLR.

This is an initial converter for named lexer tokens, character sets, string
literals, wildcards, `-> skip`, `-> channel(HIDDEN)`, lexer modes,
`-> mode(...)`, `-> type(...)`, lexer fragments and ranges, grouped alternatives,
basic repetition, and simple direct left recursion. Lexer modes become CongoCC
lexical states. A `type(TOKEN)` alias is accepted in parser rules as either the
original token or the rule that emits it, so its terminal name in a CongoCC
tree can differ from ANTLR's. The converter maps `channel(HIDDEN)` to CongoCC
`SKIP`; other channels remain unsupported. A single `.*?` in a token rule is
rendered as a CongoCC lazy token; other non-greedy lexer suffixes are rejected.
`build.sh` asks trinterp's C# choice analyzer for SCAN facts, then passes them
to the XQuery renderer. It handles a tail repetition that could consume the
start of the next enclosing rule (VMF), and overlapping alternatives made only
of terminals when the containing rule's following tokens can decide the choice
(Words). It also uses a syntactic `SCAN` for bounded, prefix-disjoint terminal
alternatives and for a repeated `rule separator` before `rule terminator`
(ABB). These are conservative cases, not a general translation of ANTLR4
adaptive prediction. For a repeated outer rule with a distinctive two-token
header, a nested repeated item can use a negative syntactic check to stop
before the next outer rule (ABNF). The left-recursion rewrite does not preserve ANTLR4 precedence or associativity. Parser rule names are preserved by default. `convert.xq` accepts the external `$prefix` parameter; `build.sh` passes `CONGOCC_RULE_PREFIX`, which defaults to the empty string. If a parser rule differs from a token name only by case, the build automatically chooses `ParserRule_` to avoid Java source-file collisions on case-insensitive filesystems. For other collisions, or a Java keyword such as `boolean` used as a parser rule name, rebuild with `CONGOCC_RULE_PREFIX=ParserRule_ bash build.sh`. The generated driver reads the prefix saved by the build, so `run.sh` needs no matching environment setting.
For an optional group of overlapping alternatives followed by one required
terminal, the converter checks each earlier branch through that terminal before
committing. This handles the instruction, directive, and label choices in
`asm/asm6502` without using lookahead measured from a particular corpus.
Unresolved overlapping top-level alternatives produce a warning during build.
The converter accepts negated lexer character sets and expands supported
ANTLR Unicode properties into explicit ranges. It rejects recursive lexer-rule
references before invoking CongoCC, whose NFA builder overflows on such cycles.
For example, the VBA6 grammar has `WS -> LINE_CONTINUATION -> WS`.
See `examples/words-congocc` and `examples/congocc-split` for runnable examples.
