package parser

import (
    "strconv"
    "github.com/antlr4-go/antlr/v4"
)


type MySQLLexerBase struct {
    *antlr.BaseLexer
    serverVersion int
    sqlModes           map[SqlMode]bool
    supportMle         bool
    charSets           map[string]bool
    inVersionComment   bool
    pendingTokens      []antlr.Token
    justEmittedDot     bool
    longString         string
    longLength         int
    signedLongString   string
    longLongString     string
    longLongLength     int
    signedLongLongString string
    signedLongLongLength int
    unsignedLongLongString string
    unsignedLongLongLength int
}

var StaticMySQLLexerBase MySQLLexerBase

func init() {
    StaticMySQLLexerBase = MySQLLexerBase {
        serverVersion:  80200,
        supportMle:     true,
        charSets:       make(map[string]bool),
        inVersionComment: false,
        longString:     "2147483647",
        longLength:     10,
        signedLongString: "-2147483648",
        longLongString: "9223372036854775807",
        longLongLength: 19,
        signedLongLongString: "-9223372036854775808",
        signedLongLongLength: 19,
        unsignedLongLongString: "18446744073709551615",
        unsignedLongLongLength: 20,
    }
	StaticMySQLLexerBase.sqlModes = sqlModeFromString("ANSI_QUOTES");
}

func NewMySQLLexerBase(input antlr.CharStream) *MySQLLexerBase {
    r := &MySQLLexerBase{
        serverVersion:  80200,
        supportMle:     true,
        charSets:       make(map[string]bool),
        inVersionComment: false,
        longString:     "2147483647",
        longLength:     10,
        signedLongString: "-2147483648",
        longLongString: "9223372036854775807",
        longLongLength: 19,
        signedLongLongString: "-9223372036854775808",
        signedLongLongLength: 19,
        unsignedLongLongString: "18446744073709551615",
        unsignedLongLongLength: 20,
    }
	r.sqlModes = sqlModeFromString("ANSI_QUOTES");
    return r
}

func (l *MySQLLexerBase) MakeCommonToken(ttype int, text string) antlr.Token {
    ctf := l.GetTokenFactory()
    t := ctf.Create(
        l.GetTokenSourceCharStreamPair(),
        ttype,
        text,
        antlr.TokenDefaultChannel,
        l.TokenStartCharIndex,
        l.TokenStartCharIndex,
        l.TokenStartLine,
        l.TokenStartColumn)
    return t
}

func (m *MySQLLexerBase) emitDot() {
    ctf := m.GetTokenFactory()
    t := ctf.Create(
        m.GetTokenSourceCharStreamPair(),
        MySQLLexerDOT_SYMBOL,
		".",
        antlr.TokenDefaultChannel,
        m.TokenStartCharIndex,
        m.TokenStartCharIndex,
        m.Interpreter.GetLine(),
        m.Interpreter.GetCharPositionInLine() - len(m.GetText()))
    m.pendingTokens = append(m.pendingTokens, t)
    m.TokenStartColumn = m.TokenStartColumn + 1
    m.TokenStartCharIndex = m.TokenStartCharIndex + 1
}

func (m *MySQLLexerBase) isMasterCompressionAlgorithm() bool { return StaticMySQLLexerBase.serverVersion >= 80018 && m.isServerVersionLt80024() }
func (m *MySQLLexerBase) isServerVersionGe80011() bool { return StaticMySQLLexerBase.serverVersion >= 80011 }
func (m *MySQLLexerBase) isServerVersionGe80013() bool { return StaticMySQLLexerBase.serverVersion >= 80013 }
func (m *MySQLLexerBase) isServerVersionLt80014() bool { return StaticMySQLLexerBase.serverVersion < 80014 }
func (m *MySQLLexerBase) isServerVersionGe80014() bool { return StaticMySQLLexerBase.serverVersion >= 80014 }
func (m *MySQLLexerBase) isServerVersionGe80016() bool { return StaticMySQLLexerBase.serverVersion >= 80016 }
func (m *MySQLLexerBase) isServerVersionGe80017() bool { return StaticMySQLLexerBase.serverVersion >= 80017 }
func (m *MySQLLexerBase) isServerVersionGe80018() bool { return StaticMySQLLexerBase.serverVersion >= 80018 }
func (m *MySQLLexerBase) isServerVersionLt80021() bool { return StaticMySQLLexerBase.serverVersion < 80021 }
func (m *MySQLLexerBase) isServerVersionGe80021() bool { return StaticMySQLLexerBase.serverVersion >= 80021 }
func (m *MySQLLexerBase) isServerVersionLt80022() bool { return StaticMySQLLexerBase.serverVersion < 80022 }
func (m *MySQLLexerBase) isServerVersionGe80022() bool { return StaticMySQLLexerBase.serverVersion >= 80022 }
func (m *MySQLLexerBase) isServerVersionLt80023() bool { return StaticMySQLLexerBase.serverVersion < 80023 }
func (m *MySQLLexerBase) isServerVersionGe80023() bool { return StaticMySQLLexerBase.serverVersion >= 80023 }
func (m *MySQLLexerBase) isServerVersionLt80024() bool { return StaticMySQLLexerBase.serverVersion < 80024 }
func (m *MySQLLexerBase) isServerVersionGe80024() bool { return StaticMySQLLexerBase.serverVersion >= 80024 }
func (m *MySQLLexerBase) isServerVersionLt80031() bool { return StaticMySQLLexerBase.serverVersion < 80031 }

func (m *MySQLLexerBase) doLogicalOr() {
    if m.isSqlModeActive(PipesAsConcat) {
        m.SetType(MySQLLexerCONCAT_PIPES_SYMBOL)
    } else {
        m.SetType(MySQLLexerLOGICAL_OR_OPERATOR)
    }
}

func (m *MySQLLexerBase) isSqlModeActive(mode SqlMode) bool { return StaticMySQLLexerBase.sqlModes[mode]; }
func (m *MySQLLexerBase) doIntNumber() { m.SetType(m.determineNumericType(m.GetText())); }

func (m *MySQLLexerBase) determineNumericType(text string) int {
    length := len(text) - 1
    if length < StaticMySQLLexerBase.longLength {
        return MySQLLexerINT_NUMBER
    }

    negative := false
    index := 0
    if text[index] == '+' {
        index++
        length--
    } else if text[index] == '-' {
        index++
        length--
        negative = true
    }
    for text[index] == '0' && length > 0 {
        index++
        length--
    }

    if length < StaticMySQLLexerBase.longLength {
        return MySQLLexerINT_NUMBER
    }

    var smaller int
    var bigger int
    var cmp string
    if negative {
        if length == StaticMySQLLexerBase.longLength {
            cmp = StaticMySQLLexerBase.signedLongString[1:]
            smaller = MySQLLexerINT_NUMBER
            bigger = MySQLLexerLONG_NUMBER
        } else if length < StaticMySQLLexerBase.signedLongLongLength {
            return MySQLLexerLONG_NUMBER;
        } else if length > StaticMySQLLexerBase.signedLongLongLength {
            return MySQLLexerDECIMAL_NUMBER;
        } else {
            cmp = StaticMySQLLexerBase.signedLongLongString[1:]
            smaller = MySQLLexerLONG_NUMBER
            bigger = MySQLLexerDECIMAL_NUMBER
        }
    } else {
        if length == StaticMySQLLexerBase.longLength {
            cmp = StaticMySQLLexerBase.longString
            smaller = MySQLLexerINT_NUMBER
            bigger =  MySQLLexerLONG_NUMBER
        } else if length < StaticMySQLLexerBase.longLongLength {
            return MySQLLexerLONG_NUMBER;
        } else if length > StaticMySQLLexerBase.longLongLength {
            if length > StaticMySQLLexerBase.unsignedLongLongLength {
                return MySQLLexerDECIMAL_NUMBER;
            }
            cmp = StaticMySQLLexerBase.unsignedLongLongString[1:]
            smaller = MySQLLexerULONGLONG_NUMBER
            bigger = MySQLLexerDECIMAL_NUMBER
        } else {
            cmp = StaticMySQLLexerBase.longLongString
            smaller = MySQLLexerLONG_NUMBER
            bigger = MySQLLexerULONGLONG_NUMBER
        }
    }

    otherIndex := 0
    for index < len(text) && cmp[otherIndex] == text[index] {
        index++
        otherIndex++
    }
    if index < len(text) {
        index++
        otherIndex++
    }
    if text[index - 1] <= cmp[otherIndex - 1] {
        return smaller
    }
    return bigger
}

func (m *MySQLLexerBase) checkMySQLVersion(text string) bool {
    if len(text) < 8 { // Minimum is: /*!12345
        return false
    }

    // Skip version comment introducer.
    version, err := strconv.Atoi(text[3:])
    if err != nil {
        return false
    }

    if version <= StaticMySQLLexerBase.serverVersion {
        StaticMySQLLexerBase.inVersionComment = true
        return true
    }

    return false
}

func (m *MySQLLexerBase) determineFunction(proposed int) int {
    // Skip any whitespace character if the sql mode says they should be ignored,
    // before actually trying to match the open parenthesis.
    input := m.GetInputStream().LA(1)
    if m.isSqlModeActive(IgnoreSpace) {
        for input == ' ' || input == '\t' || input == '\r' || input == '\n' {
            m.Interpreter.Consume(m.GetInputStream())
            // Update channel and token type
            m.SetChannel(antlr.LexerHidden)
            m.SetType(MySQLLexerWHITESPACE)
            input = m.GetInputStream().LA(1)
        }
    }

    // Determine if the next character is an open parenthesis
    if input == '(' {
        return proposed
    }
    return MySQLLexerIDENTIFIER
}

func (m *MySQLLexerBase) doAdddate() { m.SetType(m.determineFunction(MySQLLexerADDDATE_SYMBOL)) }
func (m *MySQLLexerBase) doBitAnd() { m.SetType(m.determineFunction(MySQLLexerBIT_AND_SYMBOL)) }
func (m *MySQLLexerBase) doBitOr() { m.SetType(m.determineFunction(MySQLLexerBIT_OR_SYMBOL)) }
func (m *MySQLLexerBase) doBitXor() { m.SetType(m.determineFunction(MySQLLexerBIT_XOR_SYMBOL)) }
func (m *MySQLLexerBase) doCast() { m.SetType(m.determineFunction(MySQLLexerCAST_SYMBOL)) }
func (m *MySQLLexerBase) doCount() { m.SetType(m.determineFunction(MySQLLexerCOUNT_SYMBOL)) }
func (m *MySQLLexerBase) doCurdate() { m.SetType(m.determineFunction(MySQLLexerCURDATE_SYMBOL)) }
func (m *MySQLLexerBase) doCurrentDate() { m.SetType(m.determineFunction(MySQLLexerCURDATE_SYMBOL)) }
func (m *MySQLLexerBase) doCurrentTime() { m.SetType(m.determineFunction(MySQLLexerCURTIME_SYMBOL)) }
func (m *MySQLLexerBase) doCurtime() { m.SetType(m.determineFunction(MySQLLexerCURTIME_SYMBOL)) }
func (m *MySQLLexerBase) doDateAdd() { m.SetType(m.determineFunction(MySQLLexerDATE_ADD_SYMBOL)) }
func (m *MySQLLexerBase) doDateSub() { m.SetType(m.determineFunction(MySQLLexerDATE_SUB_SYMBOL)) }
func (m *MySQLLexerBase) doExtract() { m.SetType(m.determineFunction(MySQLLexerEXTRACT_SYMBOL)) }
func (m *MySQLLexerBase) doGroupConcat() { m.SetType(m.determineFunction(MySQLLexerGROUP_CONCAT_SYMBOL)) }
func (m *MySQLLexerBase) doMax() { m.SetType(m.determineFunction(MySQLLexerMAX_SYMBOL)) }
func (m *MySQLLexerBase) doMid() { m.SetType(m.determineFunction(MySQLLexerSUBSTRING_SYMBOL)) }
func (m *MySQLLexerBase) doMin() { m.SetType(m.determineFunction(MySQLLexerMIN_SYMBOL)) }

func (m *MySQLLexerBase) doNot() {
    if m.isSqlModeActive(HighNotPrecedence) {
        m.SetType(MySQLLexerNOT2_SYMBOL)
    } else {
        m.SetType(MySQLLexerNOT_SYMBOL)
    }
}

func (m *MySQLLexerBase) doNow() { m.SetType(m.determineFunction(MySQLLexerNOW_SYMBOL)) }
func (m *MySQLLexerBase) doPosition() { m.SetType(m.determineFunction(MySQLLexerPOSITION_SYMBOL)) }
func (m *MySQLLexerBase) doSessionUser() { m.SetType(m.determineFunction(MySQLLexerUSER_SYMBOL)) }
func (m *MySQLLexerBase) doStddevSamp() { m.SetType(m.determineFunction(MySQLLexerSTDDEV_SAMP_SYMBOL)) }
func (m *MySQLLexerBase) doStddev() { m.SetType(m.determineFunction(MySQLLexerSTD_SYMBOL)) }
func (m *MySQLLexerBase) doStddevPop() { m.SetType(m.determineFunction(MySQLLexerSTD_SYMBOL)) }
func (m *MySQLLexerBase) doStd() { m.SetType(m.determineFunction(MySQLLexerSTD_SYMBOL)) }
func (m *MySQLLexerBase) doSubdate() { m.SetType(m.determineFunction(MySQLLexerSUBDATE_SYMBOL)) }
func (m *MySQLLexerBase) doSubstr() { m.SetType(m.determineFunction(MySQLLexerSUBSTRING_SYMBOL)) }
func (m *MySQLLexerBase) doSubstring() { m.SetType(m.determineFunction(MySQLLexerSUBSTRING_SYMBOL)) }
func (m *MySQLLexerBase) doSum() { m.SetType(m.determineFunction(MySQLLexerSUM_SYMBOL)) }
func (m *MySQLLexerBase) doSysdate() { m.SetType(m.determineFunction(MySQLLexerSYSDATE_SYMBOL)) }
func (m *MySQLLexerBase) doSystemUser() { m.SetType(m.determineFunction(MySQLLexerUSER_SYMBOL)) }
func (m *MySQLLexerBase) doTrim() { m.SetType(m.determineFunction(MySQLLexerTRIM_SYMBOL)) }
func (m *MySQLLexerBase) doVariance() { m.SetType(m.determineFunction(MySQLLexerVARIANCE_SYMBOL)) }
func (m *MySQLLexerBase) doVarPop() { m.SetType(m.determineFunction(MySQLLexerVARIANCE_SYMBOL)) }
func (m *MySQLLexerBase) doVarSamp() { m.SetType(m.determineFunction(MySQLLexerVAR_SAMP_SYMBOL)) }
func (m *MySQLLexerBase) doUnderscoreCharset() { m.SetType(m.checkCharset(m.GetText())) }
func (m *MySQLLexerBase) doDollarQuotedStringText() bool { return StaticMySQLLexerBase.serverVersion >= 80034 && StaticMySQLLexerBase.supportMle; }
func (m *MySQLLexerBase) isVersionComment() bool { return m.checkMySQLVersion(m.GetText()) }
func (m *MySQLLexerBase) isBackTickQuotedId() bool { return ! m.isSqlModeActive(NoBackslashEscapes) }
func (m *MySQLLexerBase) isDoubleQuotedText() bool { return !m.isSqlModeActive(NoBackslashEscapes) }
func (m *MySQLLexerBase) isSingleQuotedText() bool { return !m.isSqlModeActive(NoBackslashEscapes) }
func (m *MySQLLexerBase) startInVersionComment() { m.inVersionComment = true }
func (m *MySQLLexerBase) endInVersionComment() { m.inVersionComment = false }
func (m *MySQLLexerBase) isInVersionComment() bool { return m.inVersionComment }

/**
 * Checks if the given text corresponds to a charset defined in the server (text is preceded by an underscore).
 *
 * @param text The text to check.
 *
 * @returns UNDERSCORE_CHARSET if so, otherwise IDENTIFIER.
 */
func (m *MySQLLexerBase) checkCharset(text string) int {
    if _, ok := m.charSets[text]; ok {
        return MySQLLexerUNDERSCORE_CHARSET
    } else {
        return MySQLLexerIDENTIFIER
    }
}

/**
 * Implements the multi token feature required in our lexer.
 * A lexer rule can emit more than a single token, if needed.
 *
 * @returns The next token in the token stream.
 */
func (m *MySQLLexerBase) NextToken() antlr.Token {
    if len(m.pendingTokens) != 0 {
        pending := m.pendingTokens[0]
        m.pendingTokens = m.pendingTokens[1:]
        return pending
    }

    // Let the main lexer class run the next token recognition.
    // This might create additional tokens again.
    next := m.BaseLexer.NextToken() // Get next token
    if len(m.pendingTokens) != 0 {
        pending := m.pendingTokens[0]
        m.pendingTokens = m.pendingTokens[1:]
        m.pendingTokens = append(m.pendingTokens, next)
        return pending
    }
    return next
}
