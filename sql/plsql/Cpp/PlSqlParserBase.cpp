#include "PlSqlParserBase.h"
#include "PlSqlParser.h"

bool PlSqlParserBase::isSolidusSeparator() {
    auto* stream = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto* solidus = stream->LT(1);
    if (solidus == nullptr || solidus->getType() != PlSqlParser::SOLIDUS)
        return false;

    size_t solidusLine = solidus->getLine();

    auto* prev = stream->LT(-1);
    if (prev != nullptr && prev->getType() != antlr4::Token::EOF && prev->getLine() == solidusLine)
        return false;

    auto* next = stream->LT(2);
    if (next != nullptr && next->getType() != antlr4::Token::EOF && next->getLine() == solidusLine)
        return false;

    return true;
}

bool PlSqlParserBase::IsNotNumericFunction() {
    auto* stream = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto* lt1 = stream->LT(1);
    auto* lt2 = stream->LT(2);
    if ((lt1->getType() == PlSqlParser::SUM ||
         lt1->getType() == PlSqlParser::COUNT ||
         lt1->getType() == PlSqlParser::AVG ||
         lt1->getType() == PlSqlParser::MIN ||
         lt1->getType() == PlSqlParser::MAX ||
         lt1->getType() == PlSqlParser::ROUND ||
         lt1->getType() == PlSqlParser::LEAST ||
         lt1->getType() == PlSqlParser::GREATEST) &&
         lt2->getType() == PlSqlParser::LEFT_PAREN)
        return false;
    return true;
}

bool PlSqlParserBase::isNotStartOfJoin() {
    auto* stream = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto* lt1 = stream->LT(1);
    if (lt1->getType() == PlSqlParser::INNER ||
        lt1->getType() == PlSqlParser::CROSS ||
        lt1->getType() == PlSqlParser::NATURAL ||
        lt1->getType() == PlSqlParser::PARTITION ||
        lt1->getType() == PlSqlParser::FULL ||
        lt1->getType() == PlSqlParser::LEFT ||
        lt1->getType() == PlSqlParser::RIGHT ||
        lt1->getType() == PlSqlParser::OUTER)
        return false;
    return true;
}
