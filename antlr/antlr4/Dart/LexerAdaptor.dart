// ignore_for_file: file_names, non_constant_identifier_names, unnecessary_getters_setters

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
import "ANTLRv4Lexer.dart";

abstract class LexerAdaptor extends Lexer {
  int PREQUEL_CONSTRUCT = -10;
  int OPTIONS_CONSTRUCT = -11;

  LexerAdaptor(super.input);

  int _currentRuleType = Token.INVALID_TYPE;

  bool insideOptionsBlock = false;

  int get currentRuleType => _currentRuleType;

  set currentRuleType(int ruleType) {
    _currentRuleType = ruleType;
  }

  void handleBeginArgument() {
    if (inLexerRule()) {
      pushMode(ANTLRv4Lexer.LexerCharSet);
      more();
    } else {
      pushMode(ANTLRv4Lexer.Argument);
    }
  }

  void handleEndArgument() {
    popMode();
    type = ANTLRv4Lexer.TOKEN_ARGUMENT_CONTENT;
  }

  void handleEndAction() {
    int oldMode = mode_;
    int newMode = popMode();
    bool isActionWithinAction =
        newMode == ANTLRv4Lexer.TargetLanguageAction && oldMode == newMode;

    if (isActionWithinAction) {
      type = (ANTLRv4Lexer.TOKEN_ACTION_CONTENT);
    }
  }

  @override
  Token emit() {
    if ((type == ANTLRv4Lexer.TOKEN_OPTIONS ||
            type == ANTLRv4Lexer.TOKEN_TOKENS ||
            type == ANTLRv4Lexer.TOKEN_CHANNELS) &&
        currentRuleType == Token.INVALID_TYPE) {
      // enter prequel construct ending with an RBRACE
      currentRuleType = PREQUEL_CONSTRUCT;
    } else if (type == ANTLRv4Lexer.TOKEN_OPTIONS &&
        currentRuleType == ANTLRv4Lexer.TOKEN_TOKEN_REF) {
      currentRuleType = OPTIONS_CONSTRUCT;
    } else if (type == ANTLRv4Lexer.TOKEN_RBRACE &&
        currentRuleType == PREQUEL_CONSTRUCT) {
      // exit prequel construct
      currentRuleType = Token.INVALID_TYPE;
    } else if (type == ANTLRv4Lexer.TOKEN_RBRACE &&
        currentRuleType == OPTIONS_CONSTRUCT) {
      // exit options
      currentRuleType = ANTLRv4Lexer.TOKEN_TOKEN_REF;
    } else if (type == ANTLRv4Lexer.TOKEN_AT &&
        currentRuleType == Token.INVALID_TYPE) {
      // enter action
      currentRuleType = ANTLRv4Lexer.TOKEN_AT;
    } else if (type == ANTLRv4Lexer.TOKEN_SEMI &&
        currentRuleType == OPTIONS_CONSTRUCT) {
      // ';' in options { .... }. Don't change anything.
    } else if (type == ANTLRv4Lexer.TOKEN_END_ACTION &&
        currentRuleType == ANTLRv4Lexer.TOKEN_AT) {
      // exit action
      currentRuleType = Token.INVALID_TYPE;
    } else if (type == ANTLRv4Lexer.TOKEN_ID) {
      String firstChar = inputStream
          .getText(Interval.of(tokenStartCharIndex, tokenStartCharIndex));
      String fc = firstChar.substring(0, 1);
      if (fc == fc.toLowerCase()) {
        type = ANTLRv4Lexer.TOKEN_TOKEN_REF;
      } else {
        type = ANTLRv4Lexer.TOKEN_RULE_REF;
      }

      if (currentRuleType == Token.INVALID_TYPE) {
        // if outside of rule def
        currentRuleType = type; // set to inside lexer or parser rule
      }
    } else if (type == ANTLRv4Lexer.TOKEN_SEMI) {
      // exit rule def
      currentRuleType = Token.INVALID_TYPE;
    }

    return super.emit();
  }

  bool inLexerRule() {
    return currentRuleType == ANTLRv4Lexer.TOKEN_TOKEN_REF;
  }

  bool inParserRule() {
    // not used, but added for clarity
    return currentRuleType == ANTLRv4Lexer.TOKEN_RULE_REF;
  }

  @override
  void reset([bool resetInput = false]) {
    currentRuleType = Token.INVALID_TYPE;
    insideOptionsBlock = false;
    super.reset();
  }
}
