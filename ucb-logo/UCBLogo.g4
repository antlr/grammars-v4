/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2014 by Bart Kiers
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * Project      : logo-parser; an ANTLR4 grammar for UCB Logo
 *                https://github.com/bkiers/logo-parser
 * Developed by : Bart Kiers, bart@big-o.nl
 */
grammar UCBLogo;

@parser::header {
  import java.util.Map;
  import java.util.HashMap;
}

@parser::members {

  // A Map keeping track of all procedure (and macro) names and the amount
  // of parameters each procedure expects.
  // Taken from: http://www.cs.berkeley.edu/~bh/usermanual
  final Map<String, Integer> procedures = new HashMap<String, Integer>(){{
    put("word", 2);
    put("list", 2);
    put("sentence", 2);
    put("se", 2);
    put("fput", 2);
    put("lput", 2);
    put("array", 1);
    put("mdarray", 1);
    put("listtoarray", 1);
    put("arraytolist", 1);
    put("combine", 2);
    put("reverse", 1);
    put("gensym", 0);
    put("first", 1);
    put("firsts", 1);
    put("last", 1);
    put("butfirst", 1);
    put("bf", 1);
    put("butfirsts", 1);
    put("bfs", 1);
    put("butlast", 1);
    put("bl", 1);
    put("item", 2);
    put("mditem", 2);
    put("pick", 1);
    put("remove", 2);
    put("remdup", 1);
    put("quoted", 1);
    put("setitem", 3);
    put("mdsetitem", 3);
    put(".setfirst", 2);
    put(".setbf", 2);
    put(".setitem", 3);
    put("push", 2);
    put("pop", 1);
    put("queue", 2);
    put("dequeue", 1);
    put("wordp", 1);
    put("word?", 1);
    put("listp", 1);
    put("list?", 1);
    put("arrayp", 1);
    put("array?", 1);
    put("emptyp", 1);
    put("empty?", 1);
    put("equalp", 2);
    put("equal?", 2);
    put("notequalp", 2);
    put("notequal?", 2);
    put("beforep", 2);
    put("before?", 2);
    put(".eq", 2);
    put("memberp", 2);
    put("member?", 2);
    put("substringp", 2);
    put("substring?", 2);
    put("numberp", 1);
    put("number?", 1);
    put("vbarredp", 1);
    put("vbarred?", 1);
    put("backslashedp", 1);
    put("backslashed?", 1);
    put("count", 1);
    put("ascii", 1);
    put("rawascii", 1);
    put("char", 1);
    put("member", 2);
    put("lowercase", 1);
    put("uppercase", 1);
    put("standout", 1);
    put("parse", 1);
    put("runparse", 1);
    put("print", 1);
    put("pr", 1);
    put("type", 1);
    put("show", 1);
    put("readlist", 0);
    put("rl", 0);
    put("readword", 0);
    put("rw", 0);
    put("readrawline", 0);
    put("readchar", 0);
    put("rc", 0);
    put("readchars", 1);
    put("rcs", 1);
    put("shell", 1);
    put("setprefix", 1);
    put("prefix", 0);
    put("openread", 1);
    put("openwrite", 1);
    put("openappend", 1);
    put("openupdate", 1);
    put("close", 1);
    put("allopen", 0);
    put("closeall", 0);
    put("erasefile", 1);
    put("erf", 1);
    put("dribble", 1);
    put("nodribble", 0);
    put("setread", 1);
    put("setwrite", 1);
    put("reader", 0);
    put("writer", 0);
    put("setreadpos", 1);
    put("setwritepos", 1);
    put("readpos", 0);
    put("writepos", 0);
    put("eofp", 0);
    put("eof?", 0);
    put("filep", 1);
    put("file?", 1);
    put("keyp", 0);
    put("key?", 0);
    put("cleartext", 0);
    put("ct", 0);
    put("setcursor", 1);
    put("cursor", 0);
    put("setmargins", 1);
    put("settextcolor", 2);
    put("settc", 2);
    put("increasefont", 0);
    put("decreasefont", 0);
    put("settextsize", 1);
    put("textsize", 0);
    put("setfont", 1);
    put("font", 0);
    put("sum", 2);
    put("difference", 2);
    put("minus", 1);
    put("product", 2);
    put("quotient", 2);
    put("remainder", 2);
    put("modulo", 2);
    put("int", 1);
    put("round", 1);
    put("sqrt", 1);
    put("power", 2);
    put("exp", 1);
    put("log10", 1);
    put("ln", 1);
    put("sin", 1);
    put("radsin", 1);
    put("cos", 1);
    put("radcos", 1);
    put("arctan", 1);
    put("radarctan", 1);
    put("iseq", 2);
    put("rseq", 3);
    put("lessp", 2);
    put("less?", 2);
    put("greaterp", 2);
    put("greater?", 2);
    put("lessequalp", 2);
    put("lessequal?", 2);
    put("greaterequalp", 2);
    put("greaterequal?", 2);
    put("random", 1);
    put("rerandom", 0);
    put("form", 3);
    put("bitand", 2);
    put("bitor", 2);
    put("bitxor", 2);
    put("bitnot", 1);
    put("ashift", 2);
    put("lshift", 2);
    put("and", 2);
    put("or", 2);
    put("not", 1);
    put("forward", 1);
    put("fd", 1);
    put("back", 1);
    put("bk", 1);
    put("left", 1);
    put("lt", 1);
    put("right", 1);
    put("rt", 1);
    put("setpos", 1);
    put("setxy", 2);
    put("setx", 1);
    put("sety", 1);
    put("setheading", 1);
    put("seth", 1);
    put("home", 0);
    put("arc", 2);
    put("pos", 0);
    put("xcor", 0);
    put("ycor", 0);
    put("heading", 0);
    put("towards", 1);
    put("scrunch", 0);
    put("showturtle", 0);
    put("st", 0);
    put("hideturtle", 0);
    put("ht", 0);
    put("clean", 0);
    put("clearscreen", 0);
    put("cs", 0);
    put("wrap", 0);
    put("window", 0);
    put("fence", 0);
    put("fill", 0);
    put("filled", 2);
    put("label", 1);
    put("setlabelheight", 1);
    put("textscreen", 0);
    put("ts", 0);
    put("fullscreen", 0);
    put("fs", 0);
    put("splitscreen", 0);
    put("ss", 0);
    put("setscrunch", 2);
    put("refresh", 0);
    put("norefresh", 0);
    put("shownp", 0);
    put("shown?", 0);
    put("screenmode", 0);
    put("turtlemode", 0);
    put("labelsize", 0);
    put("pendown", 0);
    put("pd", 0);
    put("penup", 0);
    put("pu", 0);
    put("penpaint", 0);
    put("ppt", 0);
    put("penerase", 0);
    put("pe", 0);
    put("penreverse", 0);
    put("px", 0);
    put("setpencolor", 1);
    put("setpc", 1);
    put("setpalette", 2);
    put("setpensize", 1);
    put("setpenpattern", 1);
    put("setpen", 1);
    put("setbackground", 1);
    put("setbg", 1);
    put("pendownp", 0);
    put("pendown?", 0);
    put("penmode", 0);
    put("pencolor", 0);
    put("pc", 0);
    put("palette", 1);
    put("pensize", 0);
    put("penpattern", 0);
    put("pen", 0);
    put("background", 0);
    put("bg", 0);
    put("savepict", 1);
    put("loadpict", 1);
    put("epspict", 1);
    put("mousepos", 0);
    put("clickpos", 0);
    put("buttonp", 0);
    put("button?", 0);
    put("button", 0);
    put("define", 2);
    put("text", 1);
    put("fulltext", 1);
    put("copydef", 2);
    put("make", 2);
    put("name", 2);
    put("local", 1);
    put("localmake", 2);
    put("thing", 1);
    put(":quoted.varname", 0);
    put("global", 1);
    put("pprop", 3);
    put("gprop", 2);
    put("remprop", 2);
    put("plist", 1);
    put("procedurep", 1);
    put("procedure?", 1);
    put("primitivep", 1);
    put("primitive?", 1);
    put("definedp", 1);
    put("defined?", 1);
    put("namep", 1);
    put("name?", 1);
    put("plistp", 1);
    put("plist?", 1);
    put("contents", 0);
    put("buried", 0);
    put("traced", 0);
    put("stepped", 0);
    put("procedures", 0);
    put("primitives", 0);
    put("names", 0);
    put("plists", 0);
    put("namelist", 1);
    put("pllist", 1);
    put("arity", 1);
    put("nodes", 0);
    put("printout", 1);
    put("po", 1);
    put("poall", 0);
    put("pops", 0);
    put("pons", 0);
    put("popls", 0);
    put("pon", 1);
    put("popl", 1);
    put("pot", 1);
    put("pots", 0);
    put("erase", 1);
    put("er", 1);
    put("erall", 0);
    put("erps", 0);
    put("erns", 0);
    put("erpls", 0);
    put("ern", 1);
    put("erpl", 1);
    put("bury", 1);
    put("buryall", 0);
    put("buryname", 1);
    put("unbury", 1);
    put("unburyall", 0);
    put("unburyname", 1);
    put("buriedp", 1);
    put("buried?", 1);
    put("trace", 1);
    put("untrace", 1);
    put("tracedp", 1);
    put("traced?", 1);
    put("step", 1);
    put("unstep", 1);
    put("steppedp", 1);
    put("stepped?", 1);
    put("edit", 1);
    put("ed", 1);
    put("editfile", 1);
    put("edall", 0);
    put("edps", 0);
    put("edns", 0);
    put("edpls", 0);
    put("edn", 1);
    put("edpl", 1);
    put("save", 1);
    put("savel", 2);
    put("load", 1);
    put("cslsload", 1);
    put("help", 1);
    put("seteditor", 1);
    put("setlibloc", 1);
    put("sethelploc", 1);
    put("setcslsloc", 1);
    put("settemploc", 1);
    put("gc", 0);
    put(".setsegmentsize", 1);
    put("run", 1);
    put("runresult", 1);
    put("repeat", 2);
    put("forever", 1);
    put("repcount", 0);
    put("if", 2);
    put("ifelse", 3);
    put("test", 1);
    put("iftrue", 1);
    put("ift", 1);
    put("iffalse", 1);
    put("iff", 1);
    put("stop", 0);
    put("output", 1);
    put("op", 1);
    put("catch", 2);
    put("throw", 1);
    put("error", 0);
    put("pause", 0);
    put("continue", 1);
    put("co", 1);
    put("wait", 1);
    put("bye", 0);
    put(".maybeoutput", 1);
    put("goto", 1);
    put("tag", 1);
    put("ignore", 1);
    put("`", 1);
    put("for", 2);
    put("do.while", 2);
    put("while", 2);
    put("do.until", 2);
    put("until", 2);
    put("case", 2);
    put("cond", 1);
    put("apply", 2);
    put("invoke", 2);
    put("foreach", 2);
    put("map", 2);
    put("map.se", 2);
    put("filter", 2);
    put("find", 2);
    put("reduce", 2);
    put("crossmap", 2);
    put("cascade", 3);
    put("cascade.2", 5);
    put("transfer", 3);
    put(".defmacro", 2);
    put("macrop", 1);
    put("macro?", 1);
    put("macroexpand", 1);
  }};

  // A flag keeping track if the parser already looked ahead to resolve user
  // defined procedures that will be stored in the 'procedures' map.
  private boolean discoveredAllProcedures = false;

  /**
   * Creates a new instance of a {@code UCBLogoParser} where
   * any user defined procedures will be resolved in an intial
   * parse.
   *
   * @param source
   *         the UCB Logo source to parse.
   */
  public UCBLogoParser(String source) {
    this(new ANTLRInputStream(source));
  }

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

      @Override
      public void enterMacro_def(@NotNull UCBLogoParser.Macro_defContext ctx) {
        // Yes, we found a macro: save it in the procedures-map.
        procedures.put(ctx.NAME().getText(), ctx.variables.amount);
      }
    }, parser.parse());

    // Reset the input stream after having resolved the user defined procedures.
    input.reset();

    this.discoveredAllProcedures = true;
  }

  /**
   * Returns the amount of parameters the procedure expects. Note
   * that this method will only be called after {@link #procedureNameAhead()}
   * already returned {@code true}.
   *
   * @param procedureName
   *         the name of the procedure.
   *
   * @return the amount of parameters the procedure expects.
   */
  private int amountParams(String procedureName) {
    return procedures.get(procedureName.toLowerCase());
  }

  /**
   * Returns {@code true} iff the next token in the stream is of type
   * {@code NAME} and contains the text defined in {@code name}.
   *
   * @param name
   *         the text the next token should contain.
   *
   * @returns {@code true} iff the next token in the stream is of type
   * {@code NAME} and contains the text defined in {@code name}.
   */
  private boolean nameAhead(String name) {
    Token token = _input.LT(1);
    return token.getType() == NAME && token.getText().equalsIgnoreCase(name);
  }

  /**
   * Returns {@code true} iff the next token in the inout stream is of
   * type {@code NAME} and is present in the {@code procedures}.
   *
   * @returns {@code true} iff the next token in the inout stream is of
   * type {@code NAME} and is present in the {@code procedures}.
   */
  private boolean procedureNameAhead() {
    Token token = _input.LT(1);
    return token.getType() == NAME && procedures.containsKey(token.getText().toLowerCase());
  }
}

@lexer::members {

  // Counters that keep track of how deep the lexer is currently in a list
  // or array. Depending on this value, a "FOO" is either tokenized as a NAME
  // (when not inside a list or array) or else as a WORD (when inside a list
  // or array).
  private int listDepth = 0;
  private int arrayDepth = 0;
}

parse
 : instruction* EOF
 ;

instruction
 : procedure_def               #procedureDefInstruction
 | macro_def                   #macroDefInstruction
 | procedure_call_extra_input  #procedureCallExtraInputInstruction
 | procedure_call              #procedureCallInstruction
 ;

procedure_def
 : TO NAME variables body_def
   {
     procedures.put($NAME.getText(), $variables.amount);
   }
 ;

macro_def
 : MACRO NAME variables body_def
   {
     procedures.put($NAME.getText(), $variables.amount);
   }
 ;

variables returns [int amount]
 : {$amount = 0;} ( VARIABLE {$amount++;} )*
 ;

body_def
 : {discoveredAllProcedures}? body_instruction* END
 |                            ~END* END
 ;

body_instruction
 : procedure_call_extra_input
 | procedure_call
 ;

procedure_call_extra_input
 : '(' {procedureNameAhead()}? NAME expression* ')'
 ;

procedure_call
 : {procedureNameAhead()}? NAME expressions[$NAME.getText(), amountParams($NAME.getText())]
 ;

expressions[String name, int total]
locals[int n = 0]      // a counter to keep track of how many expressions we've parsed
 : (
     {$n < $total}?    // check if we've parsed enough expressions
     expression
     {$n++;}           // increments the amount of  expressions we've parsed
   )*

   {
     // Make sure there are enough inputs parsed for 'name'.
     if ($total > $n) {
       throw new RuntimeException("not enough inputs to " + name);
     }
   }
 ;

expression
 : '-' expression                #unaryMinusExpression
 | procedure_call_extra_input    #procedureCallExtraInput
 | procedure_call                #procedureCallExpression
 | '(' expression ')'            #parensExpression
 | array                         #arrayExpression
 | list                          #listExpression
 | WORD                          #wordExpression
 | QUOTED_WORD                   #quotedWordExpression
 | NUMBER                        #numberExpression
 | VARIABLE                      #variableExpression
 | NAME                          #nameExpression
 | expression '*' expression     #multiplyExpression
 | expression '/' expression     #divideExpression
 | expression '+' expression     #additionExpression
 | expression '-' expression     #subtractionExpression
 | expression '<' expression     #lessThanExpression
 | expression '>' expression     #greaterThanExpression
 | expression '<=' expression    #lessThanEqualsExpression
 | expression '>=' expression    #greaterThanEqualsExpression
 | expression '=' expression     #equalsExpression
 | expression '<>' expression    #notEqualsExpressionExpression
 ;

array
 : '{' ( ~( '{' | '}' ) | array )* '}'
 ;

list
 : '[' ( ~( '[' | ']' ) | list )* ']'
 ;

TO    : T O;
END   : E N D;
MACRO : '.' M A C R O;

WORD
 : {listDepth > 0}?  ~[ \t\r\n[\];] ( ~[ \t\r\n\];~] | LINE_CONTINUATION | '\\' ( [ \t[\]();~] | LINE_BREAK ) )*
 | {arrayDepth > 0}? ~[ \t\r\n{};]   ( ~[ \t\r\n};~]  | LINE_CONTINUATION | '\\' ( [ \t{}();~]   | LINE_BREAK ) )*
 ;

SKIP_
 : ( COMMENT | LINE_BREAK | SPACES | LINE_CONTINUATION ) -> skip
 ;

OPEN_ARRAY
 : '{' {arrayDepth++;}
 ;

CLOSE_ARRAY
 : '}' {arrayDepth--;}
 ;

OPEN_LIST
 : '[' {listDepth++;}
 ;

CLOSE_LIST
 : ']' {listDepth--;}
 ;


MINUS  : '-';
PLUS   : '+';
MULT   : '*';
DIV    : '/';
LT     : '<';
GT     : '>';
EQ     : '=';
LT_EQ  : '<=';
GT_EQ  : '>=';
NOT_EQ : '<>';

QUOTED_WORD
 : '"' ( ~[ \t\r\n[\]();~] | LINE_CONTINUATION | '\\' ( [ \t[\]();~] | LINE_BREAK ) )*
 ;

NUMBER
 : [0-9]+ ( '.' [0-9]+ )?
 ;

VARIABLE
 : ':' NAME
 ;

NAME
 : ~[-+*/=<> \t\r\n[\]()":{}] ( ~[-+*/=<> \t\r\n[\](){}] | LINE_CONTINUATION | '\\' [-+*/=<> \t\r\n[\]();~{}] )*
 ;

ANY
 : . {System.err.println("unexpected char: " + getText());}
 ;

fragment COMMENT
 : ';' ~[\r\n~]*
 ;

fragment LINE_CONTINUATION
 : COMMENT? '~' SPACES? LINE_BREAK
 ;

fragment LINE_BREAK
 : '\r'? '\n'
 | '\r'
 ;

fragment SPACES
 : [ \t]+
 ;

fragment SPACE_CHARS
 : [ \t\r\n]+
 ;

fragment A : [Aa];
fragment B : [Bb];
fragment C : [Cc];
fragment D : [Dd];
fragment E : [Ee];
fragment F : [Ff];
fragment G : [Gg];
fragment H : [Hh];
fragment I : [Ii];
fragment J : [Jj];
fragment K : [Kk];
fragment L : [Ll];
fragment M : [Mm];
fragment N : [Nn];
fragment O : [Oo];
fragment P : [Pp];
fragment Q : [Qq];
fragment R : [Rr];
fragment S : [Ss];
fragment T : [Tt];
fragment U : [Uu];
fragment V : [Vv];
fragment W : [Ww];
fragment X : [Xx];
fragment Y : [Yy];
fragment Z : [Zz];
