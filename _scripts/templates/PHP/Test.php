\<?php

require_once 'vendor/autoload.php';
<tool_grammar_tuples:{x | require_once './<x.GeneratedFileName>';
} >

use Antlr\Antlr4\Runtime\CommonTokenStream;
use Antlr\Antlr4\Runtime\Error\Listeners\DiagnosticErrorListener;
use Antlr\Antlr4\Runtime\Error\Listeners\ConsoleErrorListener;
use Antlr\Antlr4\Runtime\InputStream;
use Antlr\Antlr4\Runtime\ParserRuleContext;
use Antlr\Antlr4\Runtime\Tree\ErrorNode;
use Antlr\Antlr4\Runtime\Tree\ParseTreeListener;
use Antlr\Antlr4\Runtime\Tree\ParseTreeWalker;
use Antlr\Antlr4\Runtime\Tree\TerminalNode;

final class TreeShapeListener implements ParseTreeListener {
    public function visitTerminal(TerminalNode $node) : void {}
    public function visitErrorNode(ErrorNode $node) : void {}
    public function exitEveryRule(ParserRuleContext $ctx) : void {}

    public function enterEveryRule(ParserRuleContext $ctx) : void {
        echo $ctx->getText();
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
} else if ($input != "") {
	$str = InputStream::fromString($input);
} else if ($file_name != "") {
	$str = InputStream::fromPath($file_name);
} 
	
$lexer = new <lexer_name>($str);
if ($show_tokens) {
}
$tokens = new CommonTokenStream($lexer);
$parser = new <parser_name>($tokens);
$parser->addErrorListener(new ConsoleErrorListener());
$tree = $parser-><start_symbol>();
if ($show_tree) {
	print($tree->toStringTree($parser->getRuleNames()) . "\n");
}
