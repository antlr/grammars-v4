lexer grammar RexxLexer;

// Main rules
// %INCLUDE statement
STMT_INCLUDE                    :   Include_Statement ;
// Skippable stuff
LINE_COMMENT                    :   Line_Comment_
                                ->  channel(HIDDEN);
BLOCK_COMMENT                   :   Block_Comment_
                                ->  channel(HIDDEN);
WHISPACES                       :   Whitespaces_            -> channel(HIDDEN);
CONTINUATION                    :   Continue_               -> channel(HIDDEN);

// Keywords
KWD_ADDRESS                     :   A D D R E S S ;
KWD_ARG                         :   A R G ;
KWD_BY                          :   B Y ;
KWD_CALL                        :   C A L L ;
KWD_DIGITS                      :   D I G I T S ;
KWD_DO                          :   D O ;
KWD_DROP                        :   D R O P ;
KWD_ELSE                        :   E L S E ;
KWD_END                         :   E N D ;
KWD_ENGINEERING                 :   E N G I N E E R I N G ;
KWD_ERROR                       :   E R R O R ;
KWD_EXIT                        :   E X I T ;
KWD_EXPOSE                      :   E X P O S E ;
KWD_EXTERNAL                    :   E X T E R N A L ;
KWD_FAILURE                     :   F A I L U R E ;
KWD_FOR                         :   F O R ;
KWD_FOREVER                     :   F O R E V E R ;
KWD_FORM                        :   F O R M ;
KWD_FUZZ                        :   F U Z Z ;
KWD_HALT                        :   H A L T ;
KWD_IF                          :   I F ;
KWD_INTERPRET                   :   I N T E R P R E T ;
KWD_ITERATE                     :   I T E R A T E ;
KWD_LEAVE                       :   L E A V E ;
KWD_NAME                        :   N A M E ;
KWD_NOP                         :   N O P ;
KWD_NOVALUE                     :   N O V A L U E ;
KWD_NUMERIC                     :   N U M E R I C ;
KWD_OFF                         :   O F F ;
KWD_ON                          :   O N ;
KWD_OPTIONS                     :   O P T I O N S ;
KWD_OTHERWISE                   :   O T H E R W I S E ;
KWD_PARSE                       :   P A R S E ;
KWD_PROCEDURE                   :   P R O C E D U R E ;
KWD_PULL                        :   P U L L ;
KWD_PUSH                        :   P U S H ;
KWD_QUEUE                       :   Q U E U E ;
KWD_RETURN                      :   R E T U R N ;
KWD_SAY                         :   S A Y ;
KWD_SCIENTIFIC                  :   S C I E N T I F I C ;
KWD_SELECT                      :   S E L E C T ;
KWD_SIGNAL                      :   S I G N A L ;
KWD_SOURCE                      :   S O U R C E ;
KWD_SYNTAX                      :   S Y N T A X ;
KWD_THEN                        :   T H E N ;
KWD_TO                          :   T O ;
KWD_TRACE                       :   T R A C E ;
KWD_UNTIL                       :   U N T I L ;
KWD_UPPER                       :   U P P E R ;
KWD_VALUE                       :   V A L U E ;
KWD_VAR                         :   V A R ;
KWD_VERSION                     :   V E R S I O N ;
KWD_WHEN                        :   W H E N ;
KWD_WHILE                       :   W H I L E ;
KWD_WITH                        :   W I T H ;

// Brackets
BR_O                            :   Br_O_ ;
BR_C                            :   Br_C_ ;

// Special variables: RC, RESULT, SIGL
SPECIAL_VAR                     :   R C
                                |   R E S U L T
                                |   S I G L
                                ;
// Label, const, var, number
NUMBER                          :   Number_ ;
CONST_SYMBOL                    :   Const_symbol_ ;
VAR_SYMBOL                      :   Var_Symbol_ ;

// String and concatenation
STRING                          :   String_
// Additionally, need to consume X or B at the end not followed by
// _!?A-Za-z.#@$0-9
{int currPos = this.getCharIndex();
int textLen = super.getInputStream().size();
if (textLen > currPos) {
    if (textLen == currPos + 1) {
        if (super.getInputStream()
            .getText(
                new Interval(currPos, currPos))
            .matches("[XxBb]"))
            super.getInputStream().consume();
    } else {
        if (super.getInputStream().getText(
            new Interval(currPos, currPos + 1))
            .matches("[XxBb][^_!?A-Za-z.#@$0-9]"))
            super.getInputStream().consume();
    }
}}
                                ;
// In concatenation don't need the blanks - will be precoeesed by WHITESPACES
CONCAT                          :   VBar_ VBar_ ;

// Operations
// Assignment (also comparison and template operator)
EQ                              :   Eq_ ;
// Math: +, -, *, /, //, %, **
// Note: '+' and '-' are also used in prefix expressions and templates
PLUS                            :   Plus_ ;
MINUS                           :   Minus_ ;
MUL                             :   Asterisk_ ;
DIV                             :   Slash_ ;
QUOTINENT                       :   Percent_sign_ ;
REMAINDER                       :   Slash_ Slash_ ;
POW                             :   Asterisk_ Asterisk_ ;
// Logical: NOT, OR, AND, XOR
// Note: '\\' also used for prefix expressions
NOT                             :   Not_ ;
OR                              :   VBar_ ;
XOR                             :   Amp_ Amp_ ;
AND                             :   Amp_ ;
// Comparison
// Strict comparison: ==, \==, >>, <<, >>=, <<=, \>>, \<<
CMPS_Eq                         :   Eq_ Eq_ ;
CMPS_Neq                        :   Not_ Eq_ Eq_ ;
CMPS_M                          :   More_ More_ ;
CMPS_L                          :   Less_ Less_ ;
CMPS_MEq                        :   More_ More_ Eq_ ;
CMPS_LEq                        :   Less_ Less_ Eq_ ;
CMPS_NM                         :   Not_ More_ More_ ;
CMPS_NL                         :   Not_ Less_ Less_ ;
// Non-strict: =, \=, <>, ><, >, <, >=, <=, \>, \<
// Note: '=' is taken from Assignment (EQ)
CMP_NEq                         :   Not_ Eq_ ;
CMP_LM                          :   Less_ More_ ;
CMP_ML                          :   More_ Less_ ;
CMP_M                           :   More_ ;
CMP_L                           :   Less_ ;
CMP_MEq                         :   More_ Eq_ ;
CMP_LEq                         :   Less_ Eq_ ;
CMP_NM                          :   Not_ More_ ;
CMP_NL                          :   Not_ Less_ ;

// Additional elements
// .
STOP                            :   Stop_ ;
// ,
COMMA                           :   Comma_ ;
// :
COLON                           :   Colon_ ;
// End of line
EOL                             :   Eol_ ;
// Semicolumn
SEMICOL                         :   Scol_ ;
// --------------------------------------------------------
// Fragments
// Comments
// Include statement - need to account for this
fragment Include_Statement      :   Comment_S Bo?
                                    Percent_sign_ I N C L U D E
                                    Bo Var_Symbol_+ Bo?
                                    Comment_E
                                ;
// Line comment - no EOL allowed inside.
fragment Line_Comment_          :   Comment_S
                                    Line_Commentpart*?
                                    Asterisk_*?
                                    ( Comment_E | EOF )
                                ;
fragment Line_Commentpart       :   Line_Comment_
                                |   Slash_ Line_Comment_
                                |   Slash_ ~[*\n\r]+?
                                |   Asterisk_ ~[/\n\r]+?
                                |   ~[/*\n\r]+
                                ;
fragment Block_Comment_         :   Comment_S
                                    Block_Commentpart*?
                                    Asterisk_*?
                                    ( Comment_E | EOF )
                                ;
fragment Block_Commentpart      :   Block_Comment_
                                |   Slash_ Block_Comment_
                                |   Slash_ ~[*]+?
                                |   Asterisk_ ~[/]+?
                                |   ~[/*]+
                                ;
fragment Comment_E              :   Asterisk_ Slash_ ;
fragment Comment_S              :   Slash_ Asterisk_ ;
// Whitespaces
fragment Whitespaces_           :   Blank+ ;
// Continuation - full comments, but no EOL between
fragment Continue_              :   Comma_
                                    ( Block_Comment_ | Line_Comment_ | Blank )*?
                                    Eol_;
fragment Eol_                   :   New_Line_ Caret_Return_
                                |   Caret_Return_ New_Line_
                                |   New_Line_
                                |   Caret_Return_
                                ;
// Whitespaces
fragment Bo                     :   Blank+ ;
fragment Blank                  :   Space_
                                |   Other_blank_character
                                ;
fragment Other_blank_character  :   Form_Feed_
                                |   HTab_
                                |   VTab_
                                ;
// Label, const, var, number
// Label, var
fragment Var_Symbol_            :   General_letter Var_symbol_char*;
fragment Var_symbol_char        :   General_letter
                                |   Digit_
                                |   Stop_
                                ;
fragment General_letter         :   Underscore_
                                |   Exclamation_mark_
                                |   Question_mark_
                                |   A
                                |   B
                                |   C
                                |   D
                                |   E
                                |   F
                                |   G
                                |   H
                                |   I
                                |   J
                                |   K
                                |   L
                                |   M
                                |   N
                                |   O
                                |   P
                                |   Q
                                |   R
                                |   S
                                |   T
                                |   U
                                |   V
                                |   W
                                |   X
                                |   Y
                                |   Z
                                |   Extra_letter
                                ;
fragment Extra_letter           :   Hash_
                                |   At_
                                |   Dollar_
                                ;
// Const
fragment Const_symbol_          :   Digit_ Var_symbol_char* ;
fragment Digit_                 :   [0-9] ;
// Number
fragment Number_                :   Plain_number Exponent_? ;
fragment Plain_number           :   Digit_+ Stop_? Digit_*
                                |   Stop_ Digit_+
                                ;
fragment Exponent_              :   E ( Plus_ | Minus_ )? Digit_+ ;
// String and concatenation
fragment String_                :   Quoted_string ;
fragment Quoted_string          :   Quotation_mark_string
                                |   Apostrophe_string
                                ;
fragment Quotation_mark_string  :   Quote_ (String_char | Embedded_quotation_mark | Apostrophe_)* Quote_ ;
fragment Embedded_quotation_mark:   Quote_ Quote_ ;
fragment Apostrophe_string      :   Apostrophe_ (String_char | Embedded_apostrophe | Quote_)* Apostrophe_ ;
fragment Embedded_apostrophe    :   Apostrophe_ Apostrophe_ ;
fragment String_char            :   String_or_comment_char | Asterisk_ | Slash_ ;
fragment String_or_comment_char :   Digit_
                                |   Stop_
                                |   Special
                                |   Operator_only
                                |   General_letter
                                |   Blank
                                |   Other_character
                                ;
fragment Special                :   Comma_
                                |   Colon_
                                |   Scol_
                                |   Br_C_
                                |   Br_O_
                                ;
fragment Operator_only          :   Plus_
                                |   Minus_
                                |   Percent_sign_
                                |   VBar_
                                |   Amp_
                                |   Eq_
                                |   Not_
                                |   More_
                                |   Less_
                                ;
fragment Other_character        :   ~["'\n\r*/] ;
fragment Not_                   :   Backslash_
                                |   Other_negator
                                ;
fragment Other_negator          :   Caret_
                                |   Logical_Not_
                                |   Slash_
                                ;

// Single characters
fragment Stop_                  :   '.' ;
fragment Comma_                 :   ',' ;
fragment Colon_                 :   ':' ;
fragment Scol_                  :   ';' ;
fragment Eq_                    :   '=' ;
fragment Plus_                  :   '+' ;
fragment Minus_                 :   '-' ;
fragment Caret_                 :   '^' ;
fragment Logical_Not_           :   '¬' ;
fragment Underscore_            :   '_' ;
fragment Exclamation_mark_      :   '!' ;
fragment Question_mark_         :   '?' ;
fragment Br_O_                  :   '(' ;
fragment Br_C_                  :   ')' ;
fragment Space_                 :   ' ' ;
fragment Form_Feed_             :   '\f' ;
fragment HTab_                  :   '\t' ;
fragment VTab_                  :   '\u000b' ;
fragment Caret_Return_          :   '\r' ;
fragment New_Line_              :   '\n' ;
fragment Quote_                 :   '"' ;
fragment Apostrophe_            :   '\'' ;
fragment Slash_                 :   '/' ;
fragment Backslash_             :   '\\' ;
fragment Asterisk_              :   '*' ;
fragment More_                  :   '>' ;
fragment Less_                  :   '<' ;
fragment Percent_sign_          :   '%' ;
fragment VBar_                  :   '|' ;
fragment Amp_                   :   '&' ;
fragment Hash_                  :   '#' ;
fragment At_                    :   '@' ;
fragment Dollar_                :   '$' ;

// Letters
fragment A                      :   ('a'|'A');
fragment B                      :   ('b'|'B');
fragment C                      :   ('c'|'C');
fragment D                      :   ('d'|'D');
fragment E                      :   ('e'|'E');
fragment F                      :   ('f'|'F');
fragment G                      :   ('g'|'G');
fragment H                      :   ('h'|'H');
fragment I                      :   ('i'|'I');
fragment J                      :   ('j'|'J');
fragment K                      :   ('k'|'K');
fragment L                      :   ('l'|'L');
fragment M                      :   ('m'|'M');
fragment N                      :   ('n'|'N');
fragment O                      :   ('o'|'O');
fragment P                      :   ('p'|'P');
fragment Q                      :   ('q'|'Q');
fragment R                      :   ('r'|'R');
fragment S                      :   ('s'|'S');
fragment T                      :   ('t'|'T');
fragment U                      :   ('u'|'U');
fragment V                      :   ('v'|'V');
fragment W                      :   ('w'|'W');
fragment X                      :   ('x'|'X');
fragment Y                      :   ('y'|'Y');
fragment Z                      :   ('z'|'Z');

// Unsupported characters
UNSUPPORTED_CHARACTER           :   . ;
