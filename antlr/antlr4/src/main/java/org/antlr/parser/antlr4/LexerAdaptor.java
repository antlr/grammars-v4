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
package org.antlr.parser.antlr4;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.misc.Interval;

public abstract class LexerAdaptor extends Lexer {

    /**
     *  Generic type for OPTIONS, TOKENS and CHANNELS
     */
	private static final int PREQUEL_CONSTRUCT = -10;

	public LexerAdaptor(CharStream input) {
		super(input);
	}

	/**
	 * Track whether we are inside of a rule and whether it is lexical parser. _currentRuleType==Token.INVALID_TYPE
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
	private int _currentRuleType = Token.INVALID_TYPE;

	private boolean insideOptionsBlock = false;

	public int getCurrentRuleType() {
		return _currentRuleType;
	}

	public void setCurrentRuleType(int ruleType) {
		this._currentRuleType = ruleType;
	}

	protected void handleBeginArgument() {
		if (inLexerRule()) {
			pushMode(ANTLRv4Lexer.LexerCharSet);
			more();
		} else {
			pushMode(ANTLRv4Lexer.Argument);
		}
	}

	protected void handleEndArgument() {
		popMode();
		if (_modeStack.size() > 0) {
			setType(ANTLRv4Lexer.ARGUMENT_CONTENT);
		}
	}

	protected void handleEndAction() {
	    int oldMode = _mode;
        int newMode = popMode();
        boolean isActionWithinAction = _modeStack.size() > 0
            && newMode == ANTLRv4Lexer.Action
            && oldMode == newMode;

		if (isActionWithinAction) {
			setType(ANTLRv4Lexer.ACTION_CONTENT);
		}
	}

	protected void handleOptionsLBrace() {
		if (insideOptionsBlock) {
			setType(ANTLRv4Lexer.BEGIN_ACTION);
			pushMode(ANTLRv4Lexer.Action);
		} else {
			setType(ANTLRv4Lexer.LBRACE);
			insideOptionsBlock = true;
		}
	}

	@Override
	public Token emit() {
		if ((_type == ANTLRv4Lexer.OPTIONS || _type == ANTLRv4Lexer.TOKENS || _type == ANTLRv4Lexer.CHANNELS)
				&& _currentRuleType == Token.INVALID_TYPE) { // enter prequel construct ending with an RBRACE
			_currentRuleType = PREQUEL_CONSTRUCT;
		} else if (_type == ANTLRv4Lexer.RBRACE && _currentRuleType == PREQUEL_CONSTRUCT) { // exit prequel construct
			_currentRuleType = Token.INVALID_TYPE;
		} else if (_type == ANTLRv4Lexer.AT && _currentRuleType == Token.INVALID_TYPE) { // enter action
			_currentRuleType = ANTLRv4Lexer.AT;
		} else if (_type == ANTLRv4Lexer.END_ACTION && _currentRuleType == ANTLRv4Lexer.AT) { // exit action
			_currentRuleType = Token.INVALID_TYPE;
		} else if (_type == ANTLRv4Lexer.ID) {
			String firstChar = _input.getText(Interval.of(_tokenStartCharIndex, _tokenStartCharIndex));
			if (Character.isUpperCase(firstChar.charAt(0))) {
				_type = ANTLRv4Lexer.TOKEN_REF;
			} else {
				_type = ANTLRv4Lexer.RULE_REF;
			}

			if (_currentRuleType == Token.INVALID_TYPE) { // if outside of rule def
				_currentRuleType = _type; // set to inside lexer or parser rule
			}
		} else if (_type == ANTLRv4Lexer.SEMI) { // exit rule def
			_currentRuleType = Token.INVALID_TYPE;
		}

		return super.emit();
	}

	private boolean inLexerRule() {
		return _currentRuleType == ANTLRv4Lexer.TOKEN_REF;
	}

	@SuppressWarnings("unused")
	private boolean inParserRule() { // not used, but added for clarity
		return _currentRuleType == ANTLRv4Lexer.RULE_REF;
	}
}