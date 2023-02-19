\<?php
// Generated from trgen <version>

require_once 'vendor/autoload.php';
<tool_grammar_tuples:{x | require_once './<x.GeneratedFileName>';
} >
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
    public bool $had_error;
    public bool $quiet;
    public bool $tee;
    public mixed $output;
    public function __construct($q, $t, $o) {
        $this->output = $o;
        $this->quiet = $q;
    $this->tee = $t;
        $this->had_error = false;
    }
    public function syntaxError(
        Recognizer $recognizer,
        ?object $offendingSymbol,
        int $line,
        int $charPositionInLine,
        string $msg,
        ?RecognitionException $e
    ) : void {
        $this->had_error = true;
        if ($this->tee) {
            fwrite($this->output, sprintf("line %d:%d %s\n", $line, $charPositionInLine, $msg));
        }
        if (! $this->quiet) {
            fwrite(STDERR, sprintf("line %d:%d %s\n", $line, $charPositionInLine, $msg));
        }
    }
}

$tee = false;
$show_profile = false;
$show_tree = false;
$show_tokens = false;
$show_trace = false;
$inputs = array();
$is_fns = array();
$error_code = 0;
$string_instance = 0;
$prefix = "";
$quiet = false;

function main($argv) : void {
    global $tee;
    global $show_profile;
    global $show_tree;
    global $show_tokens;
    global $show_trace;
    global $inputs;
    global $is_fns;
    global $error_code;
    global $prefix;
    global $quiet;
    for ($i = 1; $i \< count($argv); $i++) {
        if ($argv[$i] == "-tokens") {
            $show_tokens = true;
        } else if ($argv[$i] == "-tree") {
            $show_tree = true;
        } else if ($argv[$i] == "-prefix") {
            $prefix = $argv[++$i] . " ";
        } else if ($argv[$i] == "-input") {
            array_push($inputs, $argv[++$i]);
            array_push($is_fns, false);
        } else if ($argv[$i] == "-tee") {
            $tee = true;
        } else if ($argv[$i] == "-x") {
            while($f = fgets(STDIN)){
                $f = trim($f);
                array_push($inputs, $f);
                array_push($is_fns, true);
            }
        } else if ($argv[$i] == "-q") {
            $quiet = true;
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
        for ($f = 0; $f \< count($inputs); ++$f)
        {
            if ($is_fns[$f])
                ParseFilename($inputs[$f], $f);
            else
                ParseString($inputs[$f], $f);
        }
        $duration = $timer->stop();
        if (! $quiet) {
            fwrite(STDERR, "Total Time: " . $duration->asSeconds() . "\n");
        }
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
    global $tee;
    global $show_tree;
    global $show_tokens;
    global $show_trace;
    global $inputs;
    global $is_fns;
    global $error_code;
    global $prefix;
    global $quiet;
    $lexer = new <lexer_name>($str);
    if ($show_tokens) {
        for ($i=0;  ; $i++) {
            $token = $lexer->nextToken();
            $token->setTokenIndex($i);
            fwrite(STDERR, $token . PHP_EOL);
            if ($token->getType() == Token::EOF){
                break;
            }
        }
        $lexer->reset();
    }

    if ( $tee ) {
        $output = fopen($input_name . ".errors", "w");
    } else {
        $output = STDERR;
    }
    $lexerErrorListener = new MyErrorListener($quiet, $tee, $output);
    $lexer->removeErrorListeners();
    $lexer->addErrorListener($lexerErrorListener);
    $tokens = new CommonTokenStream($lexer);
    $parser = new <parser_name>($tokens);
    $parserErrorListener = new MyErrorListener($quiet, $tee, $output);
    $parser->removeErrorListeners();
    $parser->addErrorListener($parserErrorListener);
    if ($show_trace) {
        $parser->setTrace(true);
        Antlr\Antlr4\Runtime\Atn\ParserATNSimulator::$traceAtnSimulation = true;
    }
    $timer2 = new Timer;
    $timer2->start();
    $tree = $parser-><start_symbol>();
    $duration = $timer2->stop();
    $result = "";
    if ($parserErrorListener->had_error || $lexerErrorListener->had_error) {
        $result = "fail";
        $error_code = 1;
    }
    else {
        $result = "success";
    }
    if ($show_tree) {
        if ($tee) {
            $handle = fopen($input_name . ".tree", "w");
            fprintf($handle, "%s", $tree->toStringTree($parser->getRuleNames()));
            fclose($handle);
        } else {
            fwrite(STDERR, $tree->toStringTree($parser->getRuleNames()));
        }
    }
    if ( ! $quiet ) {
        fwrite(STDERR, $prefix . "PHP " . $row_number . " " . $input_name . " " . $result . " " . $duration->asSeconds() . "\n");
    }
    if ( $tee ) {
        fclose($output);
    }
}

main($argv);
