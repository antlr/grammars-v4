\<?php

require_once 'vendor/autoload.php';
<tool_grammar_tuples:{x | require_once './<x.GeneratedFileName>';
} >
use Antlr\Antlr4\Runtime\CommonTokenStream;
use Antlr\Antlr4\Runtime\Error\Listeners\DiagnosticErrorListener;
//use Antlr\Antlr4\Runtime\Error\Listeners\ConsoleErrorListener;
use Antlr\Antlr4\Runtime\Error\Listeners\BaseErrorListener;
use Antlr\Antlr4\Runtime\Recognizer;
use Antlr\Antlr4\Runtime\Error\Exceptions\RecognitionException;
use Antlr\Antlr4\Runtime\InputStream;
use Antlr\Antlr4\Runtime\ParserRuleContext;
use Antlr\Antlr4\Runtime\Tree\ErrorNode;
use Antlr\Antlr4\Runtime\Tree\ParseTreeListener;
use Antlr\Antlr4\Runtime\Tree\ParseTreeWalker;
use Antlr\Antlr4\Runtime\Tree\TerminalNode;
use Antlr\Antlr4\Runtime\Token;
use Antlr\Antlr4\Runtime\CommonToken;
final class TreeShapeListener implements ParseTreeListener {
	public function visitTerminal(TerminalNode $node) : void {}
	public function visitErrorNode(ErrorNode $node) : void {}
	public function exitEveryRule(ParserRuleContext $ctx) : void {}
	public function enterEveryRule(ParserRuleContext $ctx) : void {
		echo $ctx->getText();
	}
}
// TODO: should ConsoleErrorListener be final?
class MyErrorListener extends BaseErrorListener /*extends ConsoleErrorListener*/ {
	public bool $noError = true;
	public function syntaxError(
		Recognizer $recognizer,
		?object $offendingSymbol,
		int $line,
		int $charPositionInLine,
		string $msg,
		?RecognitionException $e
	) : void {
		\fwrite(\STDERR, \sprintf("line %d:%d %s\n", $line, $charPositionInLine, $msg));
		//parent::syntaxError($recognizer,$offendingSymbol,$line,$charPositionInLine,$msg,$e);
		$this->noError = false;
	}
}
$show_tree = false;
$show_tokens = false;
$file_name = '';
$input = '';
$count = count($argv);
for ($i = 0; $i \< $count; $i++) {
	if ($argv[$i] == "-tokens") {
		$show_tokens = true;
		continue;
	} else if ($argv[$i] == "-tree") {
		$show_tree = true;
		continue;
	} else if ($argv[$i] == "-input") {
		$input = $argv[++$i];
	} else if ($argv[$i] == "-file") {
		$file_name = $argv[++$i];
	}
}	
if ($input == "" && $file_name == "") {
	$in = fopen("php://stdin", "r");
	$s = '';
	while (!feof($in)) {
		$s .= fread($in,65536);
	}
	$str = InputStream::fromString($s);
	fclose($in);
} else if ($input != "") {
	$str = InputStream::fromString($input);
} else if ($file_name != "") {
	$str = InputStream::fromPath($file_name);
} 
	
$lexer = new <lexer_name>($str);
if ($show_tokens) {
	for ($i=0;  ; $i++) {
		$token = $lexer->nextToken();
		$token->setTokenIndex($i);
		print($token . PHP_EOL);
		if ($token->getType() == Token::EOF){
			break;
		}
	}
	$lexer->reset();
}
$lexerErrorListener = new MyErrorListener();
$lexer->addErrorListener($lexerErrorListener);
$tokens = new CommonTokenStream($lexer);
$parser = new <parser_name>($tokens);
$parserErrorListener = new MyErrorListener();
$parser->addErrorListener($parserErrorListener);
$tree = $parser-><start_symbol>();
if ($show_tree) {
	print($tree->toStringTree($parser->getRuleNames()) . "\n");
}
if ($parserErrorListener->noError&&$lexerErrorListener->noError){
	exit(0);
}
exit(1);
