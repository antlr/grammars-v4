#include "gvprParser.h"

using namespace antlr4;

bool GvprParserBase::IsSemiRequired()
{
	auto c = this->getTokenStream()->LT(-1);
	auto d = this->getTokenStream()->LT(1);
	return c->getType() != gvprParser::CCBC;
}

bool GvprParserBase::IsSemiNotRequired()
{
	auto c = this->getTokenStream()->LT(-1);
	auto d = this->getTokenStream()->LT(1);
	return c->getType() == gvprParser::CCBC;
}
