parser grammar RexxParser;
options { tokenVocab=RexxLexer; }

file                        :   program_ EOF ;

program_                    :   ncl? instruction_list? ;
  ncl                       :   null_clause+ ;
    null_clause             :   delim+ label_list?
                            |   label_list
                            |   include_statement
                            ;
      delim                 :   SEMICOL
                            |   EOL
                            ;
      label_list            :   ( label COLON delim* )+ ;
        label               :   VAR_SYMBOL
                            |   CONST_SYMBOL
                            |   NUMBER
                            ;
      include_statement     :   STMT_INCLUDE ;
  instruction_list          :   instruction+ ;
    instruction             :   group_
                            |   single_instruction ncl?
                            ;
single_instruction          :   assignment
                            |   keyword_instruction
                            |   command_
                            ;
  assignment                :   ( VAR_SYMBOL | SPECIAL_VAR | CONST_SYMBOL ) EQ expression ;
  keyword_instruction       :   address_
                            |   arg_
                            |   call_
                            |   drop_
                            |   exit_
                            |   interpret_
                            |   iterate_
                            |   leave_
                            |   nop_
                            |   numeric_
                            |   options_
                            |   parse_
                            |   procedure_
                            |   pull_
                            |   push_
                            |   queue_
                            |   return_
                            |   say_
                            |   signal_
                            |   trace_
                            |   upper_
                            ;
  command_                  :   expression ;
group_                      :   do_
                            |   if_
                            |   select_
                            ;
do_                         :   KWD_DO do_rep? do_cond? ncl
                                       instruction_list?
                                KWD_END var_symbol? ncl? ;
  do_rep                    :   assignment do_cnt?
                            |   KWD_FOREVER
                            |   expression
                            ;
    do_cnt                  :   dot dob? dof?
                            |   dot dof? dob?
                            |   dob dot? dof?
                            |   dob dof? dot?
                            |   dof dot? dob?
                            |   dof dob? dot?
                            ;
      dot                   :   KWD_TO expression ;
      dob                   :   KWD_BY expression ;
      dof                   :   KWD_FOR expression ;
    do_cond                 :   KWD_WHILE expression
                            |   KWD_UNTIL expression
                            ;
  if_                       :   KWD_IF expression delim* then_ (delim+ else_)? ;
    then_                   :   KWD_THEN ncl? instruction ;
    else_                   :   KWD_ELSE ncl? instruction ;
  select_                   :   KWD_SELECT delim+ select_body KWD_END ncl? ;
    select_body             :   when_+ otherwise_? ;
      when_                 :   KWD_WHEN expression delim* then_ ;
      otherwise_            :   KWD_OTHERWISE delim* instruction_list? ;

/*
Note: The next part concentrates on the instructions.
It leaves unspecified the various forms of symbol, template and expression. */
address_                    :   KWD_ADDRESS
                                ( taken_constant expression? | valueexp )?
                            ;
  taken_constant            :   symbol
                            |   STRING
                            ;
  valueexp                  :   KWD_VALUE expression ;
arg_                        :   KWD_ARG template_list? ;
call_                       :   KWD_CALL ( callon_spec | function_name call_parms? ) ;
  callon_spec               :   KWD_ON callable_condition ( KWD_NAME function_name )?
                            |   KWD_OFF callable_condition
                            ;
    callable_condition      :   KWD_ERROR
                            |   KWD_FAILURE
                            |   KWD_HALT
                            ;
  call_parms                :   BR_O expression_list? BR_C
                            |   expression_list
                            ;
  expression_list           :   COMMA* expression
                                ( COMMA+ expression )* ;
drop_                       :   KWD_DROP variable_list ;
  variable_list             :   ( vref | var_symbol )+ ;
    vref                    :   BR_O var_symbol BR_C ;
    var_symbol              :   VAR_SYMBOL
                            |   SPECIAL_VAR
                            ;
exit_                       :   KWD_EXIT expression? ;
interpret_                  :   KWD_INTERPRET expression ;
iterate_                    :   KWD_ITERATE var_symbol? ;
leave_                      :   KWD_LEAVE var_symbol? ;
nop_                        :   KWD_NOP ;
numeric_                    :   KWD_NUMERIC ( numeric_digits | numeric_form | numeric_fuzz ) ;
  numeric_digits            :   KWD_DIGITS expression? ;
  numeric_form              :   KWD_FORM
                                (   KWD_ENGINEERING
                                |   KWD_SCIENTIFIC
                                |   valueexp
                                |   expression
                                )?
                            ;
  numeric_fuzz              :   KWD_FUZZ expression? ;
options_                    :   KWD_OPTIONS expression ;
parse_                      :   KWD_PARSE KWD_UPPER? parse_type template_list? ;
  parse_type                :   parse_key
                            |   parse_value
                            |   parse_var
                            ;
    parse_key               :   KWD_ARG
                            |   KWD_EXTERNAL
                            |   KWD_NUMERIC
                            |   KWD_PULL
                            |   KWD_SOURCE
                            |   KWD_VERSION
                            ;
    parse_value             :   KWD_VALUE expression? KWD_WITH ;
    parse_var               :   KWD_VAR var_symbol ;
procedure_                  :   KWD_PROCEDURE ( KWD_EXPOSE variable_list )? ;
pull_                       :   KWD_PULL template_list? ;
push_                       :   KWD_PUSH expression? ;
queue_                      :   KWD_QUEUE expression? ;
return_                     :   KWD_RETURN expression? ;
say_                        :   KWD_SAY expression? ;
signal_                     :   KWD_SIGNAL ( signal_spec | valueexp | taken_constant ) ;
  signal_spec               :   KWD_ON condition ( KWD_NAME function_name )?
                            |   KWD_OFF condition
                            ;
    condition               :   callable_condition
                            |   KWD_NOVALUE
                            |   KWD_SYNTAX
                            ;
trace_                      :   KWD_TRACE
                                (   taken_constant
                                |   valueexp
                                |   expression
                                |   KWD_ERROR
                                |   KWD_FAILURE
                                |   KWD_OFF
                                )
                            ;
upper_                      :   KWD_UPPER var_symbol+ ; // if stem -> error (cannot do 'upper j.')

/* Note: The next section describes templates. */
template_list               :   COMMA* template_ ( COMMA+ template_ )* ;
  template_                 :   ( trigger_ | target_ )+ ;
    target_                 :   VAR_SYMBOL
                            |   SPECIAL_VAR
                            |   STOP
                            ;
    trigger_                :   pattern_
                            |   positional_
                            ;
      pattern_              :   STRING
                            |   vref
                            ;
      positional_           :   absolute_positional
                            |   relative_positional
                            ;
        absolute_positional :   NUMBER
                            |   EQ position_
                            ;
          position_         :   NUMBER
                            |   vref
                            ;
      relative_positional   :   (PLUS | MINUS) position_ ;

// Note: The final part specifies the various forms of symbol, and expression.
symbol                      :   var_symbol
                            |   CONST_SYMBOL
                            |   NUMBER
                            ;
expression                  :   and_expression
                                ( or_operator and_expression )* ;
  or_operator               :   OR
                            |   XOR
                            ;
  and_expression            :   comparison ( AND comparison )* ;
comparison                  :   concatenation ( comparison_operator concatenation )* ;
  comparison_operator       :   normal_compare
                            |   strict_compare
                            ;
    normal_compare          :   EQ
                            |   CMP_NEq
                            |   CMP_LM
                            |   CMP_ML
                            |   CMP_M
                            |   CMP_L
                            |   CMP_MEq
                            |   CMP_LEq
                            |   CMP_NM
                            |   CMP_NL
                            ;
    strict_compare          :   CMPS_Eq
                            |   CMPS_Neq
                            |   CMPS_M
                            |   CMPS_L
                            |   CMPS_MEq
                            |   CMPS_LEq
                            |   CMPS_NM
                            |   CMPS_NL
                            ;
concatenation               :   addition ( CONCAT? addition )* ;
addition                    :   multiplication ( additive_operator multiplication )* ;
  additive_operator         :   PLUS
                            |   MINUS
                            ;
multiplication              :   power_expression ( multiplicative_operator power_expression )* ;
  multiplicative_operator   :   MUL
                            |   DIV
                            |   QUOTINENT
                            |   REMAINDER
                            ;
power_expression            :   prefix_expression ( POW prefix_expression )* ;
  prefix_expression         :   ( PLUS | MINUS | NOT )* term ;
    term                    :   function_
                            |   BR_O expression  BR_C
                            |   symbol
                            |   STRING
                            ;
      function_             :   function_name function_parameters ;
        function_name       :   KWD_ADDRESS
                            |   KWD_ARG
                            |   KWD_DIGITS
                            |   KWD_FORM
                            |   KWD_FUZZ
                            |   KWD_TRACE
                            |   KWD_VALUE
                            |   taken_constant
                            ;
        function_parameters :   BR_O expression_list? BR_C ;
