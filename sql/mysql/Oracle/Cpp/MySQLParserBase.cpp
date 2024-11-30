/*
 * Copyright 2024, Oracle and/or its affiliates
 */

/* eslint-disable no-underscore-dangle */
/* cspell: ignore antlr, longlong, ULONGLONG, MAXDB */

#pragma once

#include <set>
#include <string>

#include "antlr4-runtime.h"
#include "SqlMode.h"
#include "SqlModes.h"
#include "MySQLParserBase.h"


MySQLParserBase::MySQLParserBase(antlr4::TokenStream* input) : Parser(input)
{
    this->serverVersion = 80200;
    this->sqlModes = SqlModes::sqlModeFromString("ANSI_QUOTES");
}

bool MySQLParserBase::isSqlModeActive(SqlMode mode)
{
    return sqlModes.count(mode) > 0;
}

bool MySQLParserBase::isPureIdentifier()
{
    return this->isSqlModeActive(SqlMode::AnsiQuotes);
}

bool MySQLParserBase::isTextStringLiteral()
{
    return !this->isSqlModeActive(SqlMode::AnsiQuotes);
}

bool MySQLParserBase::isStoredRoutineBody()
{
    return serverVersion >= 80032 && supportMle;
}

bool MySQLParserBase::isSelectStatementWithInto()
{
    return serverVersion >= 80024 && serverVersion < 80031;
}
