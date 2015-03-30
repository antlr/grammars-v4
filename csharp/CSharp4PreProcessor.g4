/*
  Author: Christian Wulf
  E-Mail: chwchw@gmx.de

  Preprocessor semantics:
  - Chapter 9.5.1: "An implication of this is that #define and #undef directives in one
    source file have no effect on other source files in the same program."
*/

lexer grammar CSharp4PreProcessor;

import CSharp4Lexer;

//B.1.10 Pre_processing Directives
Pp_directive
  : (Pp_declaration
  | Pp_conditional
  | Pp_line
  | Pp_diagnostic
  | Pp_region
  | Pp_pragma
  ) 
  ;
fragment Pp_expression
  : WHITESPACE? Pp_or_expression WHITESPACE?
  ;
fragment Pp_or_expression
  : Pp_and_expression  WHITESPACE?
    ('||' WHITESPACE? Pp_and_expression  )*
  ;
fragment Pp_and_expression
  : Pp_equality_expression  WHITESPACE? 
    ('&&' WHITESPACE? Pp_equality_expression WHITESPACE?  )*
  ;
fragment Pp_equality_expression
  : Pp_unary_expression  WHITESPACE?
    ( '==' WHITESPACE? Pp_unary_expression WHITESPACE? 
    | '!=' WHITESPACE? Pp_unary_expression WHITESPACE? 
    )*
  ;
fragment Pp_unary_expression
  : Pp_primary_expression 
  | '!' WHITESPACE? Pp_unary_expression 
  ;
fragment Pp_primary_expression
  : TRUE 
  | FALSE 
  | Conditional_symbol 
  | '(' Pp_expression ')'
  ;
fragment Pp_declaration
  : WHITESPACE? SHARP WHITESPACE? 'define' WHITESPACE Conditional_symbol Pp_new_line
    
  | WHITESPACE? SHARP WHITESPACE? 'undef' WHITESPACE Conditional_symbol Pp_new_line
    
  ;
fragment Pp_new_line
  : WHITESPACE? SINGLE_LINE_COMMENT? NEW_LINE
  ;
// changed by chw
fragment Pp_conditional
  : Pp_if_section
  | Pp_elif_section
  | Pp_else_section
  | Pp_endif
  ;
fragment Pp_if_section
  : WHITESPACE? SHARP WHITESPACE? 'if' WHITESPACE Pp_expression Pp_new_line
  ;
fragment Pp_elif_section
  : WHITESPACE? SHARP WHITESPACE? 'elif' WHITESPACE Pp_expression Pp_new_line
  // if the if/elif-sections before has not been processed and expr is true
      
  ;
fragment Pp_else_section
  : WHITESPACE? SHARP WHITESPACE? 'else' Pp_new_line
  // if the if/elif-sections before has not been processed
      
  ;
fragment Pp_endif
  : WHITESPACE? SHARP WHITESPACE? 'endif' Pp_new_line
      
  ;
//'<Any Identifier_or_keyword Except True Or False>'
// WARNING: ignores exclusion
fragment Conditional_symbol
  : Identifier_or_keyword
  ;
fragment Pp_diagnostic
  : WHITESPACE? SHARP WHITESPACE? 'error' Pp_message
  | WHITESPACE? SHARP WHITESPACE? 'warning' Pp_message
  ;
fragment Pp_message
  : NEW_LINE
  | WHITESPACE Input_character* NEW_LINE
  ;
// changed by chw
fragment Pp_region
  : Pp_start_region
  | Pp_end_region
  ;
fragment Pp_start_region
  : WHITESPACE? SHARP WHITESPACE? 'region' Pp_message
  ;
fragment Pp_end_region
  : WHITESPACE? SHARP WHITESPACE? 'endregion' Pp_message?
  ;
fragment Pp_line
  : WHITESPACE? SHARP WHITESPACE? 'line' WHITESPACE Line_indicator Pp_new_line
  ;
fragment Line_indicator
  : Decimal_digits (WHITESPACE File_name)?
  | 'default'
  | 'hidden'
  ;
fragment File_name
  : DOUBLE_QUOTE File_name_characters DOUBLE_QUOTE
  ;
fragment File_name_characters
  : File_name_character+
  ;
//'<Any input_character Except ">'
fragment File_name_character
  : ~(["\u000D\u000A\u000D\u0085\u2028\u2029])
  ;
// We use a more flexible pragma expression that also supports C# versions below 4.0
fragment Pp_pragma
  : WHITESPACE? SHARP WHITESPACE? 'pragma' Pp_pragma_text
  ;
fragment Pp_pragma_text
  : NEW_LINE?
  | WHITESPACE Input_characters? NEW_LINE?
  ;

// -----------------------------------------------------------------------------------------------------

// the following preprocessor rules are only invoked by the extended Lexer class
// Hint: tokens need not to be skipped because they are matched within fragment rules
//   who in turn do not produce tokens by themselves
fragment SkiPped_section_part
  : WHITESPACE? SkiPped_characters? NEW_LINE
  | Pp_directive
  ;
fragment SkiPped_characters
  : Not_number_sign Input_character*
  ;
//'<Any Input_character Except #>'
// added Whitespace_character to solve warning in SkiPped_characters
fragment Not_number_sign
  : ~([#\u000D\u000A\u0085\u2028\u2029\u0009\u000B\u000C\u0020\u00A0\u1680\u180E\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2008\u2009\u200A\u202F\u3000\u205F])
  ;

