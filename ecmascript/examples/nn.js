var antlr4 = require('antlr4/index');
var ECMAScriptLexer = require('./ECMAScriptLexer').ECMAScriptLexer;
var ECMAScriptParser = require('./ECMAScriptParser').ECMAScriptParser;

var inputStream = new antlr4.InputStream("a=x(10)");
var lexer = new ECMAScriptLexer(inputStream);
var tokens = new antlr4.CommonTokenStream(lexer);
var parser = new ECMAScriptParser(tokens);
parser.buildParseTrees = true;
var tree = parser.program();
console.log(tree.toStringTree(parser));

