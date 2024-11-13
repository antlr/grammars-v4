#include "PostgreSQLParser.h"

using namespace antlr4;

void PostgreSQLParserBase::ParseRoutineBody()
{
}

bool PostgreSQLParserBase::OnlyAcceptableOps()
{
	auto c = ((CommonTokenStream*)this->getInputStream())->LT(1);
	auto text = c->getText();
	return text == "!" || text == "!!"
			|| text == "!=-" // Code for specific example.
			;
}

