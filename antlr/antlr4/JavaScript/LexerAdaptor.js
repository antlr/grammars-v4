import antlr4 from 'antlr4';
import ANTLRv4Lexer from './ANTLRv4Lexer.js';

export default class LexerAdaptor extends antlr4.Lexer
{

	constructor(input)
	{
		super(input);
		/**
		 *  Generic type for OPTIONS, TOKENS and CHANNELS
		 */
		this.PREQUEL_CONSTRUCT = -10;
		this.OPTIONS_CONSTRUCT = -11;

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
		this._currentRuleType = antlr4.Token.INVALID_TYPE;
		this.insideOptionsBlock = false;
	}

	getCurrentRuleType()
	{
		return this._currentRuleType;
	}

	setCurrentRuleType(ruleType)
	{
		this._currentRuleType = ruleType;
	}

	handleBeginArgument()
	{
		if (this.inLexerRule()) {
			this.pushMode(ANTLRv4Lexer.LexerCharSet);
			this.more();
		} else {
			this.pushMode(ANTLRv4Lexer.Argument);
		}
	}

	handleEndArgument()
	{
		this.popMode();
		if (this._modeStack.length > 0) {
			this._type = ANTLRv4Lexer.ARGUMENT_CONTENT;
		}
	}

	handleEndAction()
	{
		var oldMode = this._mode;
		var newMode = this.popMode();
		var isActionWithinAction = this._modeStack.length > 0
		   && newMode == ANTLRv4Lexer.TargetLanguageAction
		   && oldMode == newMode;

		if (isActionWithinAction) {
			this._type = ANTLRv4Lexer.ACTION_CONTENT;
		}
	}

	emit()
	{
		if ((this._type == ANTLRv4Lexer.OPTIONS || this._type == ANTLRv4Lexer.TOKENS || this._type == ANTLRv4Lexer.CHANNELS)
		    && this.getCurrentRuleType() == antlr4.Token.INVALID_TYPE) { // enter prequel construct ending with an RBRACE
			this.setCurrentRuleType(this.PREQUEL_CONSTRUCT);
		} else if (this._type == ANTLRv4Lexer.OPTIONS && this.getCurrentRuleType() == ANTLRv4Lexer.TOKEN_REF) {
			this.setCurrentRuleType(this.OPTIONS_CONSTRUCT);
		} else if (this._type == ANTLRv4Lexer.RBRACE && this.getCurrentRuleType() == this.PREQUEL_CONSTRUCT) { // exit prequel construct
			this.setCurrentRuleType(antlr4.Token.INVALID_TYPE);
		} else if (this._type == ANTLRv4Lexer.RBRACE && this.getCurrentRuleType() == this.OPTIONS_CONSTRUCT) { // exit options
			this.setCurrentRuleType(ANTLRv4Lexer.TOKEN_REF);
		} else if (this._type == ANTLRv4Lexer.AT && this.getCurrentRuleType() == antlr4.Token.INVALID_TYPE) { // enter action
			this.setCurrentRuleType(ANTLRv4Lexer.AT);
		} else if (this._type == ANTLRv4Lexer.SEMI && this.getCurrentRuleType() == this.OPTIONS_CONSTRUCT) { // ';' in options { .... }. Don't change anything.
		} else if (this._type == ANTLRv4Lexer.END_ACTION && this.getCurrentRuleType() == ANTLRv4Lexer.AT) { // exit action
			this.setCurrentRuleType(antlr4.Token.INVALID_TYPE);
		} else if (this._type == ANTLRv4Lexer.ID) {
			var firstChar = this._input.getText(this._tokenStartCharIndex, this._tokenStartCharIndex);
			var c = firstChar.charAt(0);
			if (c == c.toUpperCase()) {
				this._type = ANTLRv4Lexer.TOKEN_REF;
			} else {
				this._type = ANTLRv4Lexer.RULE_REF;
			}

			if (this.getCurrentRuleType() == antlr4.Token.INVALID_TYPE) { // if outside of rule def
				this.setCurrentRuleType(this._type); // set to inside lexer or parser rule
			}
		} else if (this._type == ANTLRv4Lexer.SEMI) { // exit rule def
			this.setCurrentRuleType(antlr4.Token.INVALID_TYPE);
		}

		return super.emit();
	}

	inLexerRule() {
		return this.getCurrentRuleType() == ANTLRv4Lexer.TOKEN_REF;
	}

	inParserRule() { // not used, but added for clarity
		return this.getCurrentRuleType() == ANTLRv4Lexer.RULE_REF;
	}

	reset()
	{
		this.setCurrentRuleType(antlr4.Token.INVALID_TYPE);
		this.insideOptionsBlock = false;
		super.reset();
	}
}
