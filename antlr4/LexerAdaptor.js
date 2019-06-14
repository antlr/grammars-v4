/*

Adapted from LexerAdaptor.py by Sebastián Gurin

You need to modify ANTLRv4Lexer.js, use it like this:

```
function ANTLRv4Lexer(input) {
  antlr4.Lexer.call(this, input) 
  this._interp = new antlr4.atn.LexerATNSimulator(this, atn, decisionsToDFA, new antlr4.PredictionContextCache());
  return this;
}
ANTLRv4Lexer.prototype = new LexerAdaptor()
ANTLRv4Lexer.prototype.constructor = ANTLRv4Lexer;
```

*/

var antlr4 = require('antlr4/index');

module.exports.LexerAdaptor = class LexerAdaptor extends antlr4.Lexer {

  getCurrentRuleType() {
    if (typeof this._currentRuleType === 'undefined') {
      this._currentRuleType = antlr4.Token.INVALID_TYPE;
    }
    return this._currentRuleType;
  }

  setCurrentRuleType(ruleType) {
    this._currentRuleType = ruleType;
  }

  handleBeginArgument() {
    if (this.inLexerRule()) {
      this.pushMode(require('./ANTLRv4Lexer').ANTLRv4Lexer.LexerCharSet);
      this.more();
    } else {
      this.pushMode(require('./ANTLRv4Lexer').ANTLRv4Lexer.Argument);
    }
  }

  handleEndArgument() {
    this.popMode();
    if (this._modeStack.length > 0) {
      this._type = require('./ANTLRv4Lexer').ANTLRv4Lexer.ARGUMENT_CONTENT;
    }
  }

  handleEndAction() {
    this.popMode();
    if (this._modeStack.length > 0) {
      this._type = require('./ANTLRv4Lexer').ANTLRv4Lexer.ACTION_CONTENT;
    }
  }

  emit() {
    if (this._type == require('./ANTLRv4Lexer').ANTLRv4Lexer.ID) {
      let firstChar = this._input.getText(this._tokenStartCharIndex, this._tokenStartCharIndex);
      if (firstChar[0].match(/[A-Z]/)) {
        this._type = require('./ANTLRv4Lexer').ANTLRv4Lexer.TOKEN_REF;
      } else {
        this._type = require('./ANTLRv4Lexer').ANTLRv4Lexer.RULE_REF;
      }
      if (this._currentRuleType == antlr4.Token.INVALID_TYPE) {
        this._currentRuleType = this._type
      }
    } else if (this._type == require('./ANTLRv4Lexer').ANTLRv4Lexer.SEMI) {
      this._currentRuleType = antlr4.Token.INVALID_TYPE;
    }
    return super.emit();
  }

  inLexerRule() {
    return this._currentRuleType == require('./ANTLRv4Lexer').ANTLRv4Lexer.TOKEN_REF;
  }

  inParserRule() { // not used, but added for clarity
    return this._currentRuleType == require('./ANTLRv4Lexer').ANTLRv4Lexer.RULE_REF;
  }
}

