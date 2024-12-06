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

bool MySQLParserBase::isServerVersionGe80004() { return this->serverVersion >= 80004; }
bool MySQLParserBase::isServerVersionGe80011() { return this->serverVersion >= 80011; }
bool MySQLParserBase::isServerVersionGe80013() { return this->serverVersion >= 80013; }
bool MySQLParserBase::isServerVersionGe80014() { return this->serverVersion >= 80014; }
bool MySQLParserBase::isServerVersionGe80016() { return this->serverVersion >= 80016; }
bool MySQLParserBase::isServerVersionGe80017() { return this->serverVersion >= 80017; }
bool MySQLParserBase::isServerVersionGe80018() { return this->serverVersion >= 80018; }
bool MySQLParserBase::isServerVersionGe80019() { return this->serverVersion >= 80019; }
bool MySQLParserBase::isServerVersionGe80024() { return this->serverVersion >= 80024; }
bool MySQLParserBase::isServerVersionGe80025() { return this->serverVersion >= 80025; }
bool MySQLParserBase::isServerVersionGe80027() { return this->serverVersion >= 80027; }
bool MySQLParserBase::isServerVersionGe80031() { return this->serverVersion >= 80031; }
bool MySQLParserBase::isServerVersionGe80032() { return this->serverVersion >= 80032; }
bool MySQLParserBase::isServerVersionGe80100() { return this->serverVersion >= 80100; }
bool MySQLParserBase::isServerVersionGe80200() { return this->serverVersion >= 80200; }
bool MySQLParserBase::isServerVersionLt80011() { return this->serverVersion < 80011; }
bool MySQLParserBase::isServerVersionLt80012() { return this->serverVersion < 80012; }
bool MySQLParserBase::isServerVersionLt80014() { return this->serverVersion < 80014; }
bool MySQLParserBase::isServerVersionLt80016() { return this->serverVersion < 80016; }
bool MySQLParserBase::isServerVersionLt80017() { return this->serverVersion < 80017; }
bool MySQLParserBase::isServerVersionLt80024() { return this->serverVersion < 80024; }
bool MySQLParserBase::isServerVersionLt80025() { return this->serverVersion < 80025; }
bool MySQLParserBase::isServerVersionLt80031() { return this->serverVersion < 80031; }
