/*
 * Copyright 2024, Oracle and/or its affiliates
 */

/* eslint-disable no-underscore-dangle */
/* cspell: ignore antlr, longlong, ULONGLONG, MAXDB */

#pragma once

#include "antlr4-runtime.h"
#include <string>
#include "SqlMode.h"
#include <queue>

/** The base lexer class provides a number of functions needed in actions in the lexer (grammar). */
class MySQLLexerBase : public antlr4::Lexer
{

    public:
        MySQLLexerBase(antlr4::CharStream * input);
        int serverVersion;
        std::set<SqlMode> sqlModes;

        /** Enable Multi Language Extension support. */
        bool supportMle;

        std::set<std::string> charSets;

    protected:
        bool inVersionComment;

    private:
        std::queue<std::unique_ptr<antlr4::Token>> pendingTokens;
        static std::string longString;
        static int longLength;
        static std::string signedLongString;
        static std::string longLongString;
        static int longLongLength;
        static std::string signedLongLongString;
        static int signedLongLongLength;
        static std::string unsignedLongLongString;
        static int unsignedLongLongLength;

        bool justEmittedDot;

        /**
         * Determines if the given SQL mode is currently active in the lexer.
         *
         * @param mode The mode to check.
         *
         * @returns True if the mode is one of the currently active modes.
         */
    public:
        bool isSqlModeActive(SqlMode mode);
        void reset();
        std::unique_ptr<antlr4::Token> nextToken() override;
        antlr4::Token * emit() override;

    protected:
        bool checkMySQLVersion(std::string text);
        int determineFunction(int proposed);
        int determineNumericType(std::string text);
        int checkCharset(std::string text);
        void emitDot();

    public:
        bool isMasterCompressionAlgorithm();
        bool isServerVersionGe80011();
        bool isServerVersionGe80013();
        bool isServerVersionLt80014();
        bool isServerVersionGe80014();
        bool isServerVersionGe80016();
        bool isServerVersionGe80017();
        bool isServerVersionGe80018();
        bool isServerVersionLt80021();
        bool isServerVersionGe80021();
        bool isServerVersionLt80022();
        bool isServerVersionGe80022();
        bool isServerVersionLt80023();
        bool isServerVersionGe80023();
        bool isServerVersionLt80024();
        bool isServerVersionGe80024();
        bool isServerVersionLt80031();
        void doLogicalOr();
        void doIntNumber();
        void doAdddate();
        void doBitAnd();
        void doBitOr();
        void doBitXor();
        void doCast();
        void doCount();
        void doCurdate();
        void doCurrentDate();
        void doCurrentTime();
        void doCurtime();
        void doDateAdd();
        void doDateSub();
        void doExtract();
        void doGroupConcat();
        void doMax();
        void doMid();
        void doMin();
        void doNot();
        void doNow();
        void doPosition();
        void doSessionUser();
        void doStddevSamp();
        void doStddev();
        void doStddevPop();
        void doStd();
        void doSubdate();
        void doSubstr();
        void doSubstring();
        void doSum();
        void doSysdate();
        void doSystemUser();
        void doTrim();
        void doVariance();
        void doVarPop();
        void doVarSamp();
        void doUnderscoreCharset();
        bool doDollarQuotedStringText();
        bool isVersionComment();
        bool isBackTickQuotedId();
        bool isDoubleQuotedText();
        bool isSingleQuotedText();
        void startInVersionComment();
        void endInVersionComment();
        bool isInVersionComment();
};
