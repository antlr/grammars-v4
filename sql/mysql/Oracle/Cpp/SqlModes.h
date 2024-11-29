/** SQL modes that control parsing behavior. */
#include once
#include "SqlMode.h"

class SqlModes {

    /**
     * Converts a mode string into individual mode flags.
     *
     * @param modes The input string to parse.
     */
	public:
		static std::set<SqlMode> sqlModeFromString(std::string modes);
}
