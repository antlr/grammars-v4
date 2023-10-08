<?php

namespace {
	use Antlr\Antlr4\Runtime\Atn\ATNDeserializer;
	use Antlr\Antlr4\Runtime\Atn\LexerATNSimulator;
	use Antlr\Antlr4\Runtime\Lexer;
	use Antlr\Antlr4\Runtime\CharStream;
	use Antlr\Antlr4\Runtime\PredictionContexts\PredictionContextCache;
	use Antlr\Antlr4\Runtime\RuleContext;
	use Antlr\Antlr4\Runtime\Atn\ATN;
	use Antlr\Antlr4\Runtime\Dfa\DFA;
	use Antlr\Antlr4\Runtime\Vocabulary;
	use Antlr\Antlr4\Runtime\RuntimeMetaData;
	use Antlr\Antlr4\Runtime\VocabularyImpl;

	class Fortran90LexerBase extends Lexer
	{
		public function __construct(CharStream $input)
		{
			parent::__construct($input);
		}

		public function IsColumnZero(): bool
		{
			return $this->getCharPositionInLine() == 0;
		}

		public function getVocabulary(): Vocabulary {
			return null;
		}

		 public function getRuleNames(): array {
			return null;
		 }

		 public function getATN(): ATN {
			return null;
		 }
	}
}
