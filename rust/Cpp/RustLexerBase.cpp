#include "RustLexerBase.h"
#include "RustLexer.h"

bool RustLexerBase::floatLiteralPossible(){
    if(this->lt1 == antlr4::Token::INVALID_TYPE || this->lt2 == antlr4::Token::INVALID_TYPE) return true;
    if(this->lt1 != RustLexer::DOT) return true;
    switch (this->lt2){
        case RustLexer::CHAR_LITERAL:
        case RustLexer::STRING_LITERAL:
        case RustLexer::RAW_STRING_LITERAL:
        case RustLexer::BYTE_LITERAL:
        case RustLexer::BYTE_STRING_LITERAL:
        case RustLexer::RAW_BYTE_STRING_LITERAL:
        case RustLexer::INTEGER_LITERAL:
        case RustLexer::DEC_LITERAL:
        case RustLexer::HEX_LITERAL:
        case RustLexer::OCT_LITERAL:
        case RustLexer::BIN_LITERAL:

        case RustLexer::KW_SUPER:
        case RustLexer::KW_SELFVALUE:
        case RustLexer::KW_SELFTYPE:
        case RustLexer::KW_CRATE:
        case RustLexer::KW_DOLLARCRATE:

        case RustLexer::GT:
        case RustLexer::RCURLYBRACE:
        case RustLexer::RSQUAREBRACKET:
        case RustLexer::RPAREN:

        case RustLexer::KW_AWAIT:

        case RustLexer::NON_KEYWORD_IDENTIFIER:
        case RustLexer::RAW_IDENTIFIER:
        case RustLexer::KW_MACRORULES:
            return false;
        default:
            return true;
    }
}
