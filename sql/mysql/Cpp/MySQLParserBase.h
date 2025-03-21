/*
 * Copyright 2024, Oracle and/or its affiliates
 */

/* eslint-disable no-underscore-dangle */
/* cspell: ignore antlr, longlong, ULONGLONG, MAXDB */

#pragma once

#include "antlr4-runtime.h"
#include <string>
#include "SqlMode.h"
#include <set>

class MySQLParserBase : public antlr4::Parser {

    // To parameterize the parsing process.
    public:
        int serverVersion;
        std::set<SqlMode> sqlModes;

    public:
        /** Enable Multi Language Extension support. */
        bool supportMle;

	protected:
        MySQLParserBase(antlr4::TokenStream* input);

    public:
        /**
         * Determines if the given SQL mode is currently active in the lexer.
         *
         * @param mode The mode to check.
         *
         * @returns True if the mode is one of the currently active modes.
         */
        bool isSqlModeActive(SqlMode mode);
        bool isPureIdentifier();
        bool isTextStringLiteral();
        bool isStoredRoutineBody();
        bool isSelectStatementWithInto();
	bool isServerVersionGe80004();
	bool isServerVersionGe80011();
	bool isServerVersionGe80013();
	bool isServerVersionGe80014();
	bool isServerVersionGe80016();
	bool isServerVersionGe80017();
	bool isServerVersionGe80018();
	bool isServerVersionGe80019();
	bool isServerVersionGe80024();
	bool isServerVersionGe80025();
	bool isServerVersionGe80027();
	bool isServerVersionGe80031();
	bool isServerVersionGe80032();
	bool isServerVersionGe80100();
	bool isServerVersionGe80200();
	bool isServerVersionLt80011();
	bool isServerVersionLt80012();
	bool isServerVersionLt80014();
	bool isServerVersionLt80016();
	bool isServerVersionLt80017();
	bool isServerVersionLt80024();
	bool isServerVersionLt80025();
	bool isServerVersionLt80031();
};
