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

import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:core';
import 'dart:convert';
import 'dart:collection';
import 'ANTLRv4Lexer.dart';

abstract class LexerAdaptor extends Lexer
{
    static const int PREQUEL_CONSTRUCT = -10;
    static const int OPTIONS_CONSTRUCT = -11;

    // I copy a reference to the stream, so It can be used as a Char Stream, not as a IISStream
    final CharStream stream;

    LexerAdaptor(CharStream input)
         : stream = input, super(input)
    {
        CurrentRuleType = Token.INVALID_TYPE;
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
    int CurrentRuleType = Token.INVALID_TYPE;

    void handleBeginArgument()
    {
        if (InLexerRule)
        {
            pushMode(ANTLRv4Lexer.LexerCharSet);
            more();
        }
        else
        {
            pushMode(ANTLRv4Lexer.Argument);
        }
    }

    void handleEndArgument()
    {
        popMode();
	bool empty = false;
	try {
		var xxx = popMode();
		pushMode(xxx);
	} catch (error, stackTrace) {
		empty = true;
	}
        if (!empty)
        {
            type = ANTLRv4Lexer.TOKEN_ARGUMENT_CONTENT;
        }
    }

    void handleEndAction()
    {
        var oldMode = mode_;
        var newMode = popMode();
	bool empty = false;
	try {
		var xxx = popMode();
		pushMode(xxx);
	} catch (error, stackTrace) {
		empty = true;
	}
        if ((!empty) && newMode == ANTLRv4Lexer.TargetLanguageAction && oldMode == newMode)
        {
            type = ANTLRv4Lexer.TOKEN_ACTION_CONTENT;
        }
    }

    bool get InLexerRule
    {
        return CurrentRuleType == ANTLRv4Lexer.TOKEN_TOKEN_REF;
    }

    bool isUpperCase(String ch) {
	return ch.length == 1 && ch == ch.toUpperCase() && ch != ch.toLowerCase();
    }

    bool isLowerCase(String ch) {
	return ch.length == 1 && ch == ch.toLowerCase() && ch != ch.toUpperCase();
    }

    @override Token emit()
    {
        if ((type == ANTLRv4Lexer.TOKEN_OPTIONS || type == ANTLRv4Lexer.TOKEN_TOKENS || type == ANTLRv4Lexer.TOKEN_CHANNELS) && CurrentRuleType == Token.INVALID_TYPE)
        {
            // enter prequel construct ending with an RBRACE
            CurrentRuleType = PREQUEL_CONSTRUCT;
        }
        else if (type == ANTLRv4Lexer.TOKEN_OPTIONS && CurrentRuleType == ANTLRv4Lexer.TOKEN_TOKEN_REF)
        {
            CurrentRuleType = OPTIONS_CONSTRUCT;
        }
        else if (type == ANTLRv4Lexer.TOKEN_RBRACE && CurrentRuleType == PREQUEL_CONSTRUCT)
        {
            // exit prequel construct
            CurrentRuleType = Token.INVALID_TYPE;
        }
        else if (type == ANTLRv4Lexer.TOKEN_RBRACE && CurrentRuleType == OPTIONS_CONSTRUCT)
        { // exit options
            CurrentRuleType = ANTLRv4Lexer.TOKEN_TOKEN_REF;
        }
        else if (type == ANTLRv4Lexer.TOKEN_AT && CurrentRuleType == Token.INVALID_TYPE)
        {
            // enter action
            CurrentRuleType = ANTLRv4Lexer.TOKEN_AT;
        }
        else if (type == ANTLRv4Lexer.TOKEN_SEMI && CurrentRuleType == OPTIONS_CONSTRUCT)
        { // ';' in options { .... }. Don't change anything.
        }
        else if (type == ANTLRv4Lexer.TOKEN_END_ACTION && CurrentRuleType == ANTLRv4Lexer.TOKEN_AT)
        {
            // exit action
            CurrentRuleType = Token.INVALID_TYPE;
        }
        else if (type == ANTLRv4Lexer.TOKEN_ID)
        {
            var firstChar = stream.getText(Interval.of(tokenStartCharIndex, tokenStartCharIndex))[0];
            if (isUpperCase(firstChar))
            {
                type = ANTLRv4Lexer.TOKEN_TOKEN_REF;
            }
            if (isLowerCase(firstChar))
            {
                type = ANTLRv4Lexer.TOKEN_RULE_REF;
            }

            if (CurrentRuleType == Token.INVALID_TYPE)
            {
                // if outside of rule def
                CurrentRuleType = type; // set to inside lexer or parser rule
            }
        }
        else if (type == ANTLRv4Lexer.TOKEN_SEMI)
        {
            CurrentRuleType = Token.INVALID_TYPE;
        }

        return super.emit();
    }

    @override void reset([bool resetInput = false])
    {
        CurrentRuleType = Token.INVALID_TYPE;
        super.reset(true);
    }
}
