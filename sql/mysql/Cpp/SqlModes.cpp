#include "SqlMode.h"
#include "SqlModes.h"

#include <set>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>

std::set<SqlMode> SqlModes::sqlModeFromString(const std::string& modes) {
	std::set<SqlMode> result;
	std::stringstream ss(modes);
	std::string mode;

	while (std::getline(ss, mode, ',')) {
		std::transform(mode.begin(), mode.end(), mode.begin(), ::toupper);
		if (mode == "ANSI" || mode == "DB2" || mode == "MAXDB" || mode == "MSSQL" ||
		    mode == "ORACLE" || mode == "POSTGRESQL") {
			result.insert({ SqlMode::AnsiQuotes, SqlMode::PipesAsConcat, SqlMode::IgnoreSpace });
		} else if (mode == "ANSI_QUOTES") {
			result.insert(SqlMode::AnsiQuotes);
		} else if (mode == "PIPES_AS_CONCAT") {
			result.insert(SqlMode::PipesAsConcat);
		} else if (mode == "NO_BACKSLASH_ESCAPES") {
			result.insert(SqlMode::NoBackslashEscapes);
		} else if (mode == "IGNORE_SPACE") {
			result.insert(SqlMode::IgnoreSpace);
		} else if (mode == "HIGH_NOT_PRECEDENCE" || mode == "MYSQL323" || mode == "MYSQL40") {
			result.insert(SqlMode::HighNotPrecedence);
		}
	}
	return result;
}

