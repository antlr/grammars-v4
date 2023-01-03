<?php

require_once 'vendor/autoload.php';
require_once './Dart2LexerBase.php';
require_once './Dart2Lexer.php';
require_once './Dart2Parser.php';

require __DIR__ . '/vendor/autoload.php';

use SebastianBergmann\Timer\Timer;
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
        \fwrite(\STDOUT, \sprintf("line %d:%d %s\n", $line, $charPositionInLine, $msg));
        //parent::syntaxError($recognizer,$offendingSymbol,$line,$charPositionInLine,$msg,$e);
        $this->noError = false;
    }
}

$show_tree = false;
$show_tokens = false;
$show_trace = false;
$inputs = array();
$is_fns = array();
$error_code = 0;
$string_instance = 0;
$prefix = "";

function main($argv) : void {
    global $show_tree;
    global $show_tokens;
    global $show_trace;
    global $inputs;
    global $is_fns;
    global $error_code;
    global $prefix;
    for ($i = 1; $i < count($argv); $i++) {
        if ($argv[$i] == "-tokens") {
            $show_tokens = true;
            continue;
        } else if ($argv[$i] == "-tree") {
            $show_tree = true;
            continue;
        } else if ($argv[$i] == "-prefix") {
            $prefix = $argv[++$i] . " ";
        } else if ($argv[$i] == "-input") {
            array_push($inputs, $argv[++$i]);
            array_push($is_fns, true);
        } else if ($argv[$i] == "-trace") {
            $show_trace = true;
        } else {
            array_push($inputs, $argv[$i]);
            array_push($is_fns, true);
        }
    }
    if (count($inputs) == 0) {
        ParseStdin();
    }
    else {
        $timer = new Timer;
        $timer->start();
        for ($f = 0; $f < count($inputs); ++$f)
        {
            if ($is_fns[$f])
                ParseFilename($inputs[$f], $f);
            else
                ParseString($inputs[$f], $f);
        }
        $duration = $timer->stop();
        fwrite(STDERR, "Total Time: " . $duration->asSeconds() . "\n");
    }
    exit($error_code);
}   

function ParseStdin() {
    $in = fopen("php://stdin", "r");
    $s = '';
    while (!feof($in)) {
        $s .= fread($in,65536);
    }
    $str = InputStream::fromString($s);
    fclose($in);
    DoParse($str, "stdin", 0);
}

function ParseString($input, $row_number) {
    global $string_instance;
    $str = InputStream::fromString($input);
    DoParse($str, "string" . $string_instance++, $row_number);
}

function ParseFilename($input, $row_number) {
    $str = InputStream::fromPath($input);
    DoParse($str, $input, $row_number);
}

function DoParse($str, $input_name, $row_number) {
    global $show_tree;
    global $show_tokens;
    global $show_trace;
    global $inputs;
    global $is_fns;
    global $error_code;
    global $prefix;
    $lexer = new Dart2Lexer($str);
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
    $lexer->removeErrorListeners();
    $lexer->addErrorListener($lexerErrorListener);
    $tokens = new CommonTokenStream($lexer);
    $parser = new Dart2Parser($tokens);
    $parserErrorListener = new MyErrorListener();
    $parser->removeErrorListeners();
    $parser->addErrorListener($parserErrorListener);
    if ($show_trace) {
        $parser->setTrace(true);
        Antlr\Antlr4\Runtime\Atn\ParserATNSimulator::$traceAtnSimulation = true;
    }
    $timer2 = new Timer;
    $timer2->start();
    $tree = $parser->compilationUnit();
    $duration = $timer2->stop();
    $result = "";
    if ($parserErrorListener->noError && $lexerErrorListener->noError) {
        $result = "success";
    }
    else {
        $result = "fail";
        $error_code = 1;
    }
    if ($show_tree) {
        print($tree->toStringTree($parser->getRuleNames()) . "\n");
    }
    fwrite(STDERR, $prefix . "PHP " . $row_number . " " . $input_name . " " . $result . " " . $duration->asSeconds() . "\n");
}

main($argv);
