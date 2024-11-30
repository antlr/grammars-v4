/** SQL modes that control parsing behavior. */
#pragma once

#include "SqlMode.h"

#include <set>
#include <string>

class SqlModes {

    /**
     * Converts a mode string into individual mode flags.
     *
     * @param modes The input string to parse.
     */
	public:
		static std::set<SqlMode> sqlModeFromString(const std::string& modes);
};
