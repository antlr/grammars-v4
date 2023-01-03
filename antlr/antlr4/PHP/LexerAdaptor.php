<?php

/*
 [The "BSD licence"]
 Copyright (c) 2005-2007 Terence Parr
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

//declare(strict_types=1);

use Antlr\Antlr4\Runtime\Lexer;
use Antlr\Antlr4\Runtime\CharStream;
use Antlr\Antlr4\Runtime\Interval;
use Antlr\Antlr4\Runtime\Token;

abstract class LexerAdaptor extends Lexer
{
    private static int $PREQUEL_CONSTRUCT = -10;
    private static int $OPTIONS_CONSTRUCT = -11;

    // I copy a reference to the stream, so It can be used as a Char Stream, not as a IISStream
    private ?CharStream $stream = null;

    public function __construct(?CharStream $input)
    {
        parent::__construct($input);
        $this->CurrentRuleType = Token::INVALID_TYPE;
	$this->_insideOptionsBlock = false;
        $this->stream = $input;
    }


    /**
     * Track whether we are inside of a rule and whether it is lexical parser. _currentRuleType==TokenConstants.InvalidType
     * means that we are outside of a rule. At the first sign of a rule name reference and _currentRuleType==invalid, we
     * can assume that we are starting a parser rule. Similarly, seeing a token reference when not already in rule means
     * starting a token rule. The terminating ';' of a rule, flips this back to invalid type.
     *
     * This is not perfect logic but works. For example, "grammar T;" means that we start and stop a lexical rule for
     * the "T;". Dangerous but works.
     *
     * The whole point of this state information is to distinguish between [..arg actions..] and [charsets]. Char sets
     * can only occur in lexical rules and arg actions cannot occur.
     */
    private int $CurrentRuleType = Token::INVALID_TYPE;

    private bool $_insideOptionsBlock;

    protected function handleBeginArgument(): void
    {
        if ($this->InLexerRule())
        {
            $this->PushMode(ANTLRv4Lexer::LexerCharSet);
            $this->More();
        }
        else
        {
            $this->PushMode(ANTLRv4Lexer::Argument);
        }
    }

    protected function handleEndArgument(): void
    {
        $this->PopMode();
        if (\count($this->modeStack) > 0)
        {
            $this->type = ANTLRv4Lexer::ARGUMENT_CONTENT;
        }
    }

    protected function handleEndAction(): void
    {
        $oldMode = $this->mode;
        $newMode = $this->PopMode();

        if (\count($this->modeStack) > 0 && $newMode == ANTLRv4Lexer::TargetLanguageAction && $oldMode == $newMode)
        {
            $this->type = ANTLRv4Lexer::ACTION_CONTENT;
        }
    }

    private function InLexerRule(): bool
    {
        return $this->CurrentRuleType == ANTLRv4Lexer::TOKEN_REF;
    }

    public function emit(): Token
    {
        if (($this->type == ANTLRv4Lexer::OPTIONS || $this->type == ANTLRv4Lexer::TOKENS || $this->type == ANTLRv4Lexer::CHANNELS) && $this->CurrentRuleType == Token::INVALID_TYPE)
        {
            // enter prequel construct ending with an RBRACE
            $this->CurrentRuleType = self::$PREQUEL_CONSTRUCT;
        }
        else if ($this->type == ANTLRv4Lexer::OPTIONS && $this->CurrentRuleType == ANTLRv4Lexer::TOKEN_REF)
        {
            $this->CurrentRuleType = self::$OPTIONS_CONSTRUCT;
        }
        else if ($this->type == ANTLRv4Lexer::RBRACE && $this->CurrentRuleType == self::$PREQUEL_CONSTRUCT)
        {
            // exit prequel construct
            $this->CurrentRuleType = Token::INVALID_TYPE;
        }
        else if ($this->type == ANTLRv4Lexer::RBRACE && $this->CurrentRuleType == self::$OPTIONS_CONSTRUCT)
        { // exit options
            $this->CurrentRuleType = ANTLRv4Lexer::TOKEN_REF;
        }
        else if ($this->type == ANTLRv4Lexer::AT && $this->CurrentRuleType == Token::INVALID_TYPE)
        {
            // enter action
            $this->CurrentRuleType = ANTLRv4Lexer::AT;
        }
        else if ($this->type == ANTLRv4Lexer::SEMI && $this->CurrentRuleType == self::$OPTIONS_CONSTRUCT)
        { // ';' in options { .... }. Don't change anything.
        }
        else if ($this->type == ANTLRv4Lexer::END_ACTION && $this->CurrentRuleType == ANTLRv4Lexer::AT)
        {
            // exit action
            $this->CurrentRuleType = Token::INVALID_TYPE;
        }
        else if ($this->type == ANTLRv4Lexer::ID)
        {
            $start = $this->tokenStartCharIndex;
            //$interv = new Interval($start, $start);
            $text = $this->stream->getText($start, $start);
	    $firstChar = $text[0];
            if (ctype_upper($firstChar))
            {
                $this->type = ANTLRv4Lexer::TOKEN_REF;
            }
            if (ctype_lower($firstChar))
            {
                $this->type = ANTLRv4Lexer::RULE_REF;
            }

            if ($this->CurrentRuleType == Token::INVALID_TYPE)
            {
                // if outside of rule def
                $this->CurrentRuleType = $this->type; // set to inside lexer or parser rule
            }
        }
        else if ($this->type == ANTLRv4Lexer::SEMI)
        {
            $this->CurrentRuleType = Token::INVALID_TYPE;
        }

        return parent::Emit();
    }

    public function Reset(): void
    {
        $this->CurrentRuleType = Token::INVALID_TYPE;
        $this->_insideOptionsBlock = false;
	parent::Reset();
    }
}
