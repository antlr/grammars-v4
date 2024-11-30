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
};
