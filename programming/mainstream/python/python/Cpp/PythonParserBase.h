#pragma once

#include "antlr4-runtime.h"

class PythonParserBase : public antlr4::Parser {
	public:
		enum PythonVersion
		{
			Autodetect,
			Python2 = 2,
			Python3 = 3
		};
		PythonParserBase(antlr4::TokenStream *input);
		bool CheckVersion(int version);
		void SetVersion(int requiredVersion);
		int Version;
};
