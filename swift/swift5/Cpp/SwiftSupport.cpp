#include "SwiftSupport.h"
#include "Swift5Parser.h"
#include "Swift5Lexer.h"
#include "antlr4-runtime.h"

using namespace antlr4;

static int utf8_first_char_len(const char *s, int len) {
    int i;
    if(len <= 1) return len;
    for(i = 1; i < len; i++) {
        if((s[i] & 0xc0) != 0x80) {
            break;
        }
    }
    return i;
}

static int utf8_char_to_ucs4(const char *s, int len) {
    switch(len) {
        case 0: return 0;
        case 1: return *s;
        default: {
            int mask = (1 << (8 - len)) - 1;                // first utf8 code point data mask e.g len = 2 => 00111111 for 110XXXXX
            int value = *s & mask;
            for (++s, --len; len != 0; s++, len--) {
                value <<= 6;
                value += (*s & 0x3F);
            }
            return value;
        }
    }
}

template<typename BITSET>
static bool isCharacterFromSet(std::unique_ptr<antlr4::Token>& token, const BITSET& bitSet) {
    if (token->getType() == Token::EOF) {
        return false;
    } else {
        String text = token->getText();
        int first_char_len = utf8_first_char_len(text.data(), text.size());
        if (first_char_len != text.size()) {
            // not a single character
            return false;
        } else {
            return bitSet.get(utf8_char_to_ucs4(text.data(), first_char_len));
        }
    }
}

const SwiftSupport::OperatorHead SwiftSupport::operatorHead;
const SwiftSupport::OperatorCharacter SwiftSupport::operatorCharacter;
const SwiftSupport::LeftWS SwiftSupport::leftWS;
const SwiftSupport::RightWS SwiftSupport::rightWS;

SwiftSupport::OperatorHead::OperatorHead() {
    // operator-head → /  =­  -­  +­  !­  *­  %­  <­  >­  &­  |­  ^­  ~­  ?­
    set('/');
    set('=');
    set('-');
    set('+');
    set('!');
    set('*');
    set('%');
    set('&');
    set('|');
    set('<');
    set('>');
    set('^');
    set('~');
    set('?');

    // operator-head → U+00A1–U+00A7
    set(0x00A1, 0x00A7 + 1);

    // operator-head → U+00A9 or U+00AB
    set(0x00A9);
    set(0x00AB);

    // operator-head → U+00AC or U+00AE
    set(0x00AC);
    set(0x00AE);

    // operator-head → U+00B0–U+00B1, U+00B6, U+00BB, U+00BF, U+00D7, or U+00F7
    set(0x00B0, 0x00B1 + 1);
    set(0x00B6);
    set(0x00BB);
    set(0x00BF);
    set(0x00D7);
    set(0x00F7);

    // operator-head → U+2016–U+2017 or U+2020–U+2027
    set(0x2016, 0x2017 + 1);
    set(0x2020, 0x2027 + 1);

    // operator-head → U+2030–U+203E
    set(0x2030, 0x203E + 1);

    // operator-head → U+2041–U+2053
    set(0x2041, 0x2053 + 1);

    // operator-head → U+2055–U+205E
    set(0x2055, 0x205E + 1);

    // operator-head → U+2190–U+23FF
    set(0x2190, 0x23FF + 1);

    // operator-head → U+2500–U+2775
    set(0x2500, 0x2775 + 1);

    // operator-head → U+2794–U+2BFF
    set(0x2794, 0x2BFF + 1);

    // operator-head → U+2E00–U+2E7F
    set(0x2E00, 0x2E7F + 1);

    // operator-head → U+3001–U+3003
    set(0x3001, 0x3003 + 1);

    // operator-head → U+3008–U+3030
    set(0x3008, 0x3020 + 1);

    set(0x3030);
}

SwiftSupport::OperatorCharacter::OperatorCharacter()
    : OperatorHead() {
    // operator-character → operator-head­

    // operator-character → U+0300–U+036F
    set(0x0300, 0x036F + 1);
    // operator-character → U+1DC0–U+1DFF
    set(0x1DC0, 0x1DFF + 1);
    // operator-character → U+20D0–U+20FF
    set(0x20D0, 0x20FF + 1);
    // operator-character → U+FE00–U+FE0F
    set(0xFE00, 0xFE0F + 1);
    // operator-character → U+FE20–U+FE2F
    set(0xFE20, 0xFE2F + 1);

    // operator-character → U+E0100–U+E01EF
    // Java works with 16-bit unicode chars. However, it can work for targets in other languages, e.g. in Swift
    set(0xE0100,0xE01EF+1);
}

SwiftSupport::LeftWS::LeftWS() {
    set(Swift5Parser::WS);
    set(Swift5Parser::LPAREN);
    set(Swift5Parser::Interpolataion_multi_line);
    set(Swift5Parser::Interpolataion_single_line);
    set(Swift5Parser::LBRACK);
    set(Swift5Parser::LCURLY);
    set(Swift5Parser::COMMA);
    set(Swift5Parser::COLON);
    set(Swift5Parser::SEMI);
}


SwiftSupport::RightWS::RightWS() {
    set(Swift5Parser::WS);
    set(Swift5Parser::RPAREN);
    set(Swift5Parser::RBRACK);
    set(Swift5Parser::RCURLY);
    set(Swift5Parser::COMMA);
    set(Swift5Parser::COLON);
    set(Swift5Parser::SEMI);
    set(Swift5Parser::Line_comment);
    set(Swift5Parser::Block_comment);
}

bool SwiftSupport::isOperatorHead(std::unique_ptr<antlr4::Token>& token) {
    return isCharacterFromSet(token, operatorHead);
}

bool SwiftSupport::isOperatorCharacter(std::unique_ptr<antlr4::Token>& token) {
    return isCharacterFromSet(token, operatorCharacter);
}

bool SwiftSupport::isOpNext(antlr4::TokenStream* tokens) {
    return getLastOpTokenIndex(tokens) != -1;
}

/**
* Find stop token index of next operator; return -1 if not operator.
*/
int SwiftSupport::getLastOpTokenIndex(antlr4::TokenStream* tokens) {
    fillUp(tokens);
    int currentTokenIndex = tokens->index(); // current on-channel lookahead token index
    unowned_unique_ptr<Token> currentToken = tokens->get(currentTokenIndex);

    //System.out.println("getLastOpTokenIndex: "+currentToken.getText());


    // operator → dot-operator-head­ dot-operator-characters
    if (currentToken->getType() == Swift5Parser::DOT && tokens->get(currentTokenIndex + 1)->getType() == Swift5Parser::DOT) {
        //System.out.println("DOT");


        // dot-operator
        currentTokenIndex += 2; // point at token after ".."
        currentToken = tokens->get(currentTokenIndex);

        // dot-operator-character → .­ | operator-character­
        while (currentToken->getType() == Swift5Parser::DOT || isOperatorCharacter(currentToken)) {
            //System.out.println("DOT");
            currentTokenIndex++;
            currentToken = tokens->get(currentTokenIndex);
        }

        //System.out.println("result: "+(currentTokenIndex - 1));
        return currentTokenIndex - 1;
    }

    // operator → operator-head­ operator-characters­?

    if (isOperatorHead(currentToken)) {
        //System.out.println("isOperatorHead");

        currentToken = tokens->get(currentTokenIndex);
        while (isOperatorCharacter(currentToken)) {
            //System.out.println("isOperatorCharacter");
            currentTokenIndex++;
            currentToken = tokens->get(currentTokenIndex);
        }
        //System.out.println("result: "+(currentTokenIndex - 1));
        return currentTokenIndex - 1;
    } else {
        //System.out.println("result: "+(-1));
        return -1;
    }
}

/**
* "If an operator has whitespace around both sides or around neither side,
* it is treated as a binary operator. As an example, the + operator in a+b
* and a + b is treated as a binary operator."
*/
bool SwiftSupport::isBinaryOp(antlr4::TokenStream* tokens) {
    fillUp(tokens);
    int stop = getLastOpTokenIndex(tokens);
    if (stop == -1) return false;

    int start = tokens->index();
    unowned_unique_ptr<Token> currentToken = tokens->get(start);
    unowned_unique_ptr<Token> prevToken = tokens->get(start - 1); // includes hidden-channel tokens
    unowned_unique_ptr<Token> nextToken = tokens->get(stop + 1);
    bool prevIsWS = isLeftOperatorWS(prevToken);
    bool nextIsWS = isRightOperatorWS(nextToken);
    //String text = tokens.getText(Interval.of(start, stop));
    //System.out.println("isBinaryOp: '"+prevToken+"','"+text+"','"+nextToken+"' is "+result);
    if (prevIsWS) {
        return nextIsWS;
    } else {
        if (currentToken->getType() == Swift5Lexer::BANG || currentToken->getType() == Swift5Lexer::QUESTION) {
            return false;
        } else {
            if (!nextIsWS) return nextToken->getType() != Swift5Lexer::DOT;
        }
    }
    return false;
}

/**
* "If an operator has whitespace on the left side only, it is treated as a
* prefix unary operator. As an example, the ++ operator in a ++b is treated
* as a prefix unary operator."
*/
bool SwiftSupport::isPrefixOp(antlr4::TokenStream* tokens) {
    fillUp(tokens);
    int stop = getLastOpTokenIndex(tokens);
    if (stop == -1) return false;

    int start = tokens->index();
    unowned_unique_ptr<Token> prevToken = tokens->get(start - 1); // includes hidden-channel tokens
    unowned_unique_ptr<Token> nextToken = tokens->get(stop + 1);
    bool prevIsWS = isLeftOperatorWS(prevToken);
    bool nextIsWS = isRightOperatorWS(nextToken);
    //String text = tokens.getText(Interval.of(start, stop));
    // System.out.println("isPrefixOp: '"+prevToken+"','"+text+"','"+nextToken+"' is "+result);
    return prevIsWS && !nextIsWS;
}

/**
* "If an operator has whitespace on the right side only, it is treated as a
* postfix unary operator. As an example, the ++ operator in a++ b is treated
* as a postfix unary operator."
* <p>
* "If an operator has no whitespace on the left but is followed immediately
* by a dot (.), it is treated as a postfix unary operator. As an example,
* the ++ operator in a++.b is treated as a postfix unary operator (a++ .b
* rather than a ++ .b)."
*/
bool SwiftSupport::isPostfixOp(antlr4::TokenStream* tokens) {
    fillUp(tokens);
    int stop = getLastOpTokenIndex(tokens);
    if (stop == -1) return false;

    int start = tokens->index();
    unowned_unique_ptr<Token> prevToken = tokens->get(start - 1); // includes hidden-channel tokens
    unowned_unique_ptr<Token> nextToken = tokens->get(stop + 1);
    bool prevIsWS = isLeftOperatorWS(prevToken);
    bool nextIsWS = isRightOperatorWS(nextToken);
    //String text = tokens.getText(Interval.of(start, stop));
    // System.out.println("isPostfixOp: '"+prevToken+"','"+text+"','"+nextToken+"' is "+result);
    return !prevIsWS && nextIsWS ||
            !prevIsWS && nextToken->getType() == Swift5Parser::DOT;
}

bool SwiftSupport::isOperator(antlr4::TokenStream* tokens, const char* op) {
    fillUp(tokens);
    ssize_t stop = getLastOpTokenIndex(tokens);
    if (stop == -1) return false;

    ssize_t start = tokens->index();
    String text = tokens->getText(misc::Interval(start, stop));
    // System.out.println("text: '"+text+"', op: '"+op+"', text.equals(op): '"+text.equals(op)+"'");

    // for (int i = 0; i <= stop; i++) {
    //     System.out.println("token["+i+"] = '"+tokens.getText(Interval.of(i, i))+"'");
    // }

    return text.equals(op);
}

bool SwiftSupport::isLeftOperatorWS(std::unique_ptr<antlr4::Token>& t) {
    return leftWS.get(t->getType());
}

bool SwiftSupport::isRightOperatorWS(std::unique_ptr<antlr4::Token>& t) {
    return rightWS.get(t->getType()) || t->getType() == Token::EOF;
}

bool SwiftSupport::isSeparatedStatement(antlr4::TokenStream* tokens, int indexOfPreviousStatement) {
    fillUp(tokens);
    //System.out.println("------");
    //System.out.println("indexOfPreviousStatement: " + indexOfPreviousStatement);

    int indexFrom = indexOfPreviousStatement - 1;
    int indexTo = tokens->index() - 1;

    if (indexFrom >= 0) {
        // Stupid check for new line and semicolon, can be optimized
        while (indexFrom >= 0 && tokens->get(indexFrom)->getChannel() == Token::HIDDEN_CHANNEL) {
            indexFrom--;
        }

        //System.out.println("from: '" + tokens.getText(Interval.of(indexFrom, indexFrom))+"', "+tokens.get(indexFrom));
        //System.out.println("to: '" + tokens.getText(Interval.of(indexTo, indexTo))+"', "+tokens.get(indexTo));
        //System.out.println("in_between: '" + tokens.getText(Interval.of(indexFrom, indexTo)));

        //for (int i = previousIndex; i < currentIndex; i++)
        for(int i =indexTo;i>= indexFrom;i--){
            String t = tokens->get(i)->getText();
            if(t.contains("\n") || t.contains(";")){
                return true;
            }
        }
        return false;
        //String text = tokens.getText(Interval.of(indexFrom, indexTo));
        //return text.contains("\n") || text.contains(";");
    } else {
        return true;
    }
}

void SwiftSupport::fillUp(antlr4::TokenStream* tokens) {
    for (int jj = 1;;++jj)
    {
        int t = tokens->LA(jj);
        if (t == -1) break;
    }
}
