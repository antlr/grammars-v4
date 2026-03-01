#include "PlSqlParserBase.h"
#include "PlSqlParser.h"

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
