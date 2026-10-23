/*
BSD License

Copyright (c) 2026, Tom Everett
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*
* https://esolangs.org/wiki/Reaper
*
* Reaper uses Python-like significant indentation to delimit the body of a
* class definition. INDENT/DEDENT tokens are synthesized by the lexer (see
* @lexer::members below), the same technique used by this repo's python3
* grammar, just inlined into this combined grammar instead of a separate
* lexer base class.
*
* A bodyless class reference (the wiki's "class-prototype", e.g. the final
* bare "Cat" in the wiki's example program) is simply a class_declaration
* with no following INDENT, which already unambiguously distinguishes it
* from class_definition (which requires INDENT destructor DEDENT) without
* needing a separate REDENT token.
*/
grammar reaper;

tokens {
   INDENT,
   DEDENT,
   SEP
}

@lexer::members {
   private java.util.LinkedList<Token> pendingTokens = new java.util.LinkedList<>();
   private java.util.Deque<Integer> indents = new java.util.ArrayDeque<>();

   @Override
   public void emit(Token t) {
      super.setToken(t);
      pendingTokens.offer(t);
   }

   @Override
   public Token nextToken() {
      if (_input.LA(1) == EOF && !indents.isEmpty()) {
         for (int i = pendingTokens.size() - 1; i >= 0; i--) {
            if (pendingTokens.get(i).getType() == EOF) {
               pendingTokens.remove(i);
            }
         }
         while (!indents.isEmpty()) {
            emit(createIndentToken(reaperParser.DEDENT, ""));
            indents.pop();
         }
         emit(createIndentToken(Token.EOF, "<EOF>"));
      }
      Token next = super.nextToken();
      return pendingTokens.isEmpty() ? next : pendingTokens.poll();
   }

   private CommonToken createIndentToken(int type, String text) {
      int stop = getCharIndex() - 1;
      int start = text.isEmpty() ? stop : stop - text.length() + 1;
      return new CommonToken(_tokenFactorySourcePair, type, DEFAULT_TOKEN_CHANNEL, start, stop);
   }

   private static int indentWidth(String spaces) {
      int count = 0;
      for (int i = 0; i < spaces.length(); i++) {
         char ch = spaces.charAt(i);
         if (ch == '\t') {
            count += 8 - (count % 8);
         } else {
            count++;
         }
      }
      return count;
   }

   void onNewline() {
      String spaces = getText().replaceAll("[\r\n]+", "");
      int next = _input.LA(1);
      if (next == '\r' || next == '\n') {
         skip();
         return;
      }
      if (next == EOF) {
         while (!indents.isEmpty()) {
            emit(createIndentToken(reaperParser.DEDENT, ""));
            indents.pop();
         }
         skip();
         return;
      }
      int indent = indentWidth(spaces);
      int previous = indents.isEmpty() ? 0 : indents.peek();
      if (indent == previous) {
         emit(createIndentToken(reaperParser.SEP, ""));
      } else if (indent > previous) {
         indents.push(indent);
         emit(createIndentToken(reaperParser.INDENT, spaces));
      } else {
         while (!indents.isEmpty() && indents.peek() > indent) {
            emit(createIndentToken(reaperParser.DEDENT, ""));
            indents.pop();
         }
         skip();
      }
   }
}

program_
   : destructor EOF
   ;

destructor
   : statement+
   ;

statement
   : SEP? (class_definition | class_prototype | expression)
   ;

class_definition
   : class_declaration INDENT destructor DEDENT
   ;

class_prototype
   : class_declaration
   ;

class_declaration
   : constructor parameter_list
   ;

parameter_list
   : (variable | '(' parameter_list ')')*
   ;

expression
   : expression ('=' | ':=') expression
   | constructor expression_list
   | variable
   ;

expression_list
   : (expression | '(' expression_list ')')*
   ;

constructor
   : IDENTIFIER
   | NUMBER
   | STRING
   ;

variable
   : IDENTIFIER
   ;

NUMBER
   : [0-9]+
   ;

STRING
   : '"' ~ '"'* '"'
   ;

IDENTIFIER
   : [a-zA-Z] [a-zA-Z0-9.\-_]*
   ;

COMMENT
   : '--' ~ [\r\n]* -> skip
   ;

NEWLINE
   : ('\r'? '\n' [ \t]*) { onNewline(); }
   ;

WS
   : [ \t]+ -> skip
   ;
