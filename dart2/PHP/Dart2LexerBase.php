<?php

//declare(strict_types=1);

use Antlr\Antlr4\Runtime\Lexer;

abstract class Dart2LexerBase extends Lexer
{
        public function __constructor($input)
        {
                parent::_construct($input);
        }

        public function CheckNotOpenBrace(): bool
        {
                return $this->input->LA(1) != '{';
        }
}
