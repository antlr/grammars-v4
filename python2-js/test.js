
var antlr4 = require('antlr4');
var Python2Lexer = require('./Python2Lexer');
var Python2Parser = require('./Python2Parser');

function parser(__input) {
  var chars = new antlr4.InputStream(__input);
  var lexer = new Python2Lexer.Python2Lexer(chars);
  var tokens = new antlr4.CommonTokenStream(lexer);
  var parser = new Python2Parser.Python2Parser(tokens);
  var tree = parser.file_input();
}


parser(`
def printme( str ):
  print str;
  return;

printme("hello world");
printme("hello world";
`)
