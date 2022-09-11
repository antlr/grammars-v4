#include "PythonParserBase.h"

using namespace antlr4;

PythonParserBase::PythonParserBase(antlr4::TokenStream *input) : Parser(input)
{
	Version = PythonVersion::Autodetect;
}

bool PythonParserBase::CheckVersion(int version)
{
	return Version == PythonVersion::Autodetect || version == (int) Version;
}

void PythonParserBase::SetVersion(int requiredVersion)
{
	Version = (PythonVersion) requiredVersion;
}
