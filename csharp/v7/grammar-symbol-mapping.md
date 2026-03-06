## Grammar Symbol Mapping

Mapping between ANTLR4 grammar rule names and their corresponding symbols in the ECMA-334 7th edition (December 2023) specification. The **Section** column gives the `Source` entry from the spec EBNF.

Rules marked *(ANTLR-specific)* have no direct single spec equivalent.

### CSharpParser rules

| ANTLR4 Rule | Spec Symbol | Section |
|---|---|---|
| `compilation_unit` | `compilation_unit` | §14.2 |
| `namespace_or_type_name` | `namespace_or_type_name` | §7.8.1 |
| `type_` | `type` | §8.1 |
| `base_type` | *(ANTLR-specific)* | |
| `tuple_type` | `tuple_type` | §8.3.1 |
| `tuple_element` | `tuple_type_element` | §8.3.1 |
| `simple_type` | `simple_type` | §8.3.1 |
| `numeric_type` | `numeric_type` | §8.3.1 |
| `integral_type` | `integral_type` | §8.3.1 |
| `floating_point_type` | `floating_point_type` | §8.3.1 |
| `class_type` | `class_type` | §8.2.1 |
| `type_argument_list` | `type_argument_list` | §8.4.2 |
| `argument_list` | `argument_list` | §12.6.2.1 |
| `argument` | `argument` | §12.6.2.1 |
| `expression` | `expression` | §12.22 |
| `non_assignment_expression` | `non_assignment_expression` | §12.22 |
| `assignment` | `assignment` | §12.21.1 |
| `assignment_operator` | `assignment_operator` | §12.21.1 |
| `conditional_expression` | `conditional_expression` | §12.18 |
| `null_coalescing_expression` | `null_coalescing_expression` | §12.15 |
| `conditional_or_expression` | `conditional_or_expression` | §12.14.1 |
| `conditional_and_expression` | `conditional_and_expression` | §12.14.1 |
| `inclusive_or_expression` | `inclusive_or_expression` | §12.13.1 |
| `exclusive_or_expression` | `exclusive_or_expression` | §12.13.1 |
| `and_expression` | `and_expression` | §12.13.1 |
| `equality_expression` | `equality_expression` | §12.12.1 |
| `relational_expression` | `relational_expression` | §12.12.1 |
| `shift_expression` | `shift_expression` | §12.11 |
| `additive_expression` | `additive_expression` | §12.10.1 |
| `multiplicative_expression` | `multiplicative_expression` | §12.10.1 |
| `unary_expression` | `unary_expression` | §12.9.1 |
| `cast_expression` | `cast_expression` | §12.9.7 |
| `primary_expression` | `primary_expression` | §12.8.1 |
| `primary_expression_start` | `primary_no_array_creation_expression` | §12.8.1 |
| `throwable_expression` | *(ANTLR-specific)* | |
| `throw_expression` | `throw_expression` | §12.16 |
| `member_access` | `member_access` | §12.8.7.1 |
| `bracket_expression` | `element_access` | §12.8.11.1 |
| `indexer_argument` | `argument` | §12.6.2.1 |
| `predefined_type` | `predefined_type` | §12.8.7.1 |
| `expression_list` | `expression_list` | §12.8.16.4 |
| `object_or_collection_initializer` | `object_or_collection_initializer` | §12.8.16.2 |
| `object_initializer` | `object_initializer` | §12.8.16.3 |
| `member_initializer_list` | `member_initializer_list` | §12.8.16.3 |
| `member_initializer` | `member_initializer` | §12.8.16.3 |
| `initializer_value` | `initializer_value` | §12.8.16.3 |
| `collection_initializer` | `collection_initializer` | §12.8.16.4 |
| `element_initializer` | `element_initializer` | §12.8.16.4 |
| `anonymous_object_initializer` | `anonymous_object_initializer` | §12.8.16.7 |
| `member_declarator_list` | `member_declarator_list` | §12.8.16.7 |
| `member_declarator` | `member_declarator` | §12.8.16.7 |
| `unbound_type_name` | `unbound_type_name` | §12.8.17 |
| `generic_dimension_specifier` | `generic_dimension_specifier` | §12.8.17 |
| `lambda_expression` | `lambda_expression` | §12.19.1 |
| `anonymous_function_signature` | `anonymous_function_signature` | §12.19.1 |
| `explicit_anonymous_function_parameter_list` | `explicit_anonymous_function_parameter_list` | §12.19.1 |
| `explicit_anonymous_function_parameter` | `explicit_anonymous_function_parameter` | §12.19.1 |
| `implicit_anonymous_function_parameter_list` | `implicit_anonymous_function_parameter_list` | §12.19.1 |
| `anonymous_function_body` | `anonymous_function_body` | §12.19.1 |
| `query_expression` | `query_expression` | §12.20.1 |
| `from_clause` | `from_clause` | §12.20.1 |
| `query_body` | `query_body` | §12.20.1 |
| `query_body_clause` | `query_body_clause` | §12.20.1 |
| `let_clause` | `let_clause` | §12.20.1 |
| `where_clause` | `where_clause` | §12.20.1 |
| `combined_join_clause` | `join_clause` / `join_into_clause` | §12.20.1 |
| `orderby_clause` | `orderby_clause` | §12.20.1 |
| `ordering` | `ordering` | §12.20.1 |
| `select_or_group_clause` | `select_or_group_clause` | §12.20.1 |
| `query_continuation` | `query_continuation` | §12.20.1 |
| `statement` | `statement` | §13.1 |
| `declarationStatement` | `declaration_statement` | §13.6.1 |
| `local_function_declaration` | `local_function_declaration` | §13.6.4 |
| `local_function_header` | `local_function_header` | §13.6.4 |
| `local_function_modifiers` | `local_function_modifier` | §13.6.4 |
| `local_function_body` | `local_function_body` | §13.6.4 |
| `labeled_Statement` | `labeled_statement` | §13.5 |
| `embedded_statement` | `embedded_statement` | §13.1 |
| `simple_embedded_statement` | *(ANTLR-specific — covers §13.4, §13.7–§13.15)* | |
| `block` | `block` | §13.3.1 |
| `local_variable_declaration` | `local_variable_declaration` | §13.6.2 |
| `local_variable_type` | `local_variable_type` | §12.17 |
| `local_variable_declarator` | `explicitly_typed_local_variable_declarator` | §13.6.2.2 |
| `local_variable_initializer` | `local_variable_initializer` | §13.6.2.2 |
| `local_constant_declaration` | `local_constant_declaration` | §13.6.3 |
| `if_body` | *(ANTLR-specific)* | |
| `switch_section` | `switch_section` | §13.8.3 |
| `switch_label` | `switch_label` | §13.8.3 |
| `case_guard` | `case_guard` | §13.8.3 |
| `pattern` | `pattern` | §11.2.1 |
| `simple_designation` | *(part of `declaration_pattern` / `var_pattern`)* | §11.2.2 / §11.2.4 |
| `statement_list` | `statement_list` | §13.3.2 |
| `for_initializer` | `for_initializer` | §13.9.4 |
| `for_iterator` | `for_iterator` | §13.9.4 |
| `catch_clauses` | `catch_clauses` | §13.11 |
| `specific_catch_clause` | `specific_catch_clause` | §13.11 |
| `general_catch_clause` | `general_catch_clause` | §13.11 |
| `exception_filter` | `exception_filter` | §13.11 |
| `finally_clause` | `finally_clause` | §13.11 |
| `resource_acquisition` | `resource_acquisition` | §13.14 |
| `namespace_declaration` | `namespace_declaration` | §14.3 |
| `qualified_identifier` | `qualified_identifier` | §14.3 |
| `namespace_body` | `namespace_body` | §14.3 |
| `extern_alias_directives` | *(ANTLR-specific)* | |
| `extern_alias_directive` | `extern_alias_directive` | §14.4 |
| `using_directives` | *(ANTLR-specific)* | |
| `using_directive` | `using_directive` | §14.5.1 |
| `namespace_member_declarations` | *(ANTLR-specific)* | |
| `namespace_member_declaration` | `namespace_member_declaration` | §14.6 |
| `type_declaration` | `type_declaration` | §14.7 |
| `qualified_alias_member` | `qualified_alias_member` | §14.8.1 |
| `type_parameter_list` | `type_parameter_list` | §15.2.3 |
| `type_parameter` | `type_parameter` | §8.5 |
| `class_base` | `class_base` | §15.2.4.1 |
| `interface_type_list` | `interface_type_list` | §15.2.4.1 |
| `type_parameter_constraints_clauses` | `type_parameter_constraints_clauses` | §15.2.5 |
| `type_parameter_constraints_clause` | `type_parameter_constraints_clause` | §15.2.5 |
| `type_parameter_constraints` | `type_parameter_constraints` | §15.2.5 |
| `primary_constraint` | `primary_constraint` | §15.2.5 |
| `secondary_constraints` | `secondary_constraints` | §15.2.5 |
| `constructor_constraint` | `constructor_constraint` | §15.2.5 |
| `class_body` | `class_body` | §15.2.6 |
| `class_member_declarations` | *(ANTLR-specific)* | |
| `class_member_declaration` | `class_member_declaration` | §15.3.1 |
| `all_member_modifiers` | *(ANTLR-specific)* | |
| `all_member_modifier` | *(ANTLR-specific — combines class / struct / interface modifiers)* | |
| `common_member_declaration` | *(ANTLR-specific)* | |
| `typed_member_declaration` | *(ANTLR-specific)* | |
| `constant_declarators` | `constant_declarators` | §13.6.3 |
| `constant_declarator` | `constant_declarator` | §13.6.3 |
| `variable_declarators` | `variable_declarators` | §15.5.1 |
| `variable_declarator` | `variable_declarator` | §15.5.1 |
| `variable_initializer` | `variable_initializer` | §17.7 |
| `return_type` | `return_type` | §15.6.1 |
| `member_name` | `member_name` | §15.6.1 |
| `method_body` | `method_body` | §15.6.1 |
| `formal_parameter_list` | `formal_parameter_list` | §15.6.2.1 |
| `fixed_parameters` | `fixed_parameters` | §15.6.2.1 |
| `fixed_parameter` | `fixed_parameter` | §15.6.2.1 |
| `parameter_modifier` | `parameter_modifier` | §15.6.2.1 |
| `parameter_array` | `parameter_array` | §15.6.2.1 |
| `accessor_declarations` | `accessor_declarations` | §15.7.3 |
| `get_accessor_declaration` | `get_accessor_declaration` | §15.7.3 |
| `set_accessor_declaration` | `set_accessor_declaration` | §15.7.3 |
| `accessor_modifier` | `accessor_modifier` | §15.7.3 |
| `accessor_body` | `accessor_body` | §15.7.3 |
| `event_accessor_declarations` | `event_accessor_declarations` | §15.8.1 |
| `add_accessor_declaration` | `add_accessor_declaration` | §15.8.1 |
| `remove_accessor_declaration` | `remove_accessor_declaration` | §15.8.1 |
| `overloadable_operator` | `overloadable_unary_operator` / `overloadable_binary_operator` | §15.10.1 |
| `conversion_operator_declarator` | `conversion_operator_declarator` | §15.10.1 |
| `constructor_initializer` | `constructor_initializer` | §15.11.1 |
| `body` | *(ANTLR-specific — unifies method/accessor/operator bodies)* | |
| `struct_interfaces` | `struct_interfaces` | §16.2.5 |
| `struct_body` | `struct_body` | §16.2.6 |
| `struct_member_declaration` | `struct_member_declaration` | §16.3 |
| `array_type` | `array_type` | §8.2.1 |
| `rank_specifier` | `rank_specifier` | §8.2.1 |
| `array_initializer` | `array_initializer` | §17.7 |
| `variant_type_parameter_list` | `variant_type_parameter_list` | §18.2.3.1 |
| `variant_type_parameter` | `variant_type_parameters` | §18.2.3.1 |
| `variance_annotation` | `variance_annotation` | §18.2.3.1 |
| `interface_base` | `interface_base` | §18.2.4 |
| `interface_body` | `interface_body` | §18.3 |
| `interface_member_declaration` | `interface_member_declaration` | §18.4.1 |
| `interface_accessors` | `interface_accessors` | §18.4.3 |
| `enum_base` | `enum_base` | §19.2 |
| `enum_body` | `enum_body` | §19.2 |
| `enum_member_declaration` | `enum_member_declaration` | §19.4 |
| `global_attribute_section` | `global_attribute_section` | §22.3 |
| `global_attribute_target` | `global_attribute_target` | §22.3 |
| `attributes` | `attributes` | §22.3 |
| `attribute_section` | `attribute_section` | §22.3 |
| `attribute_target` | `attribute_target` | §22.3 |
| `attribute_list` | `attribute_list` | §22.3 |
| `attribute` | `attribute` | §22.3 |
| `attribute_argument` | `positional_argument` / `named_argument` | §22.3 |
| `pointer_type` | `pointer_type` | §23.3 |
| `fixed_pointer_declarators` | `fixed_pointer_declarators` | §23.7 |
| `fixed_pointer_declarator` | `fixed_pointer_declarator` | §23.7 |
| `fixed_pointer_initializer` | `fixed_pointer_initializer` | §23.7 |
| `fixed_size_buffer_declarator` | `fixed_size_buffer_declarator` | §23.8.2 |
| `stackalloc_initializer` | `stackalloc_initializer` | §12.8.21 |
| `right_arrow` | *(ANTLR-specific syntactic predicate)* | |
| `right_shift` | `right_shift` | §6.4.6 |
| `right_shift_assignment` | `right_shift_assignment` | §6.4.6 |
| `literal` | `literal` | §6.4.5.1 |
| `boolean_literal` | `boolean_literal` | §6.4.5.2 |
| `string_literal` | `String_Literal` | §6.4.5.6 |
| `interpolated_regular_string` | `interpolated_regular_string_expression` | §12.8.3 |
| `interpolated_verbatium_string` | `interpolated_verbatim_string_expression` | §12.8.3 |
| `interpolated_regular_string_part` | `Interpolated_Regular_String_Element` | §12.8.3 |
| `interpolated_verbatium_string_part` | `Interpolated_Verbatim_String_Element` | §12.8.3 |
| `interpolated_string_expression` | `regular_interpolation` / `verbatim_interpolation` | §12.8.3 |
| `keyword` | `keyword` | §6.4.4 |
| `class_definition` | `class_declaration` | §15.2.1 |
| `struct_definition` | `struct_declaration` | §16.2.1 |
| `interface_definition` | `interface_declaration` | §18.2.1 |
| `enum_definition` | `enum_declaration` | §19.2 |
| `delegate_definition` | `delegate_declaration` | §20.2 |
| `event_declaration` | `event_declaration` | §15.8.1 |
| `field_declaration` | `field_declaration` | §15.5.1 |
| `property_declaration` | `property_declaration` | §15.7.1 |
| `constant_declaration` | `constant_declaration` | §15.4 |
| `indexer_declaration` | `indexer_declaration` | §15.9.1 |
| `destructor_definition` | `finalizer_declaration` | §15.13 |
| `constructor_declaration` | `constructor_declaration` | §15.11.1 |
| `method_declaration` | `method_declaration` | §15.6.1 |
| `method_member_name` | *(ANTLR-specific)* | |
| `operator_declaration` | `operator_declaration` | §15.10.1 |
| `arg_declaration` | `fixed_parameter` | §15.6.2.1 |
| `method_invocation` | `invocation_expression` | §12.8.9.1 |
| `object_creation_expression` | `object_creation_expression` | §12.8.16.2 |
| `identifier` | `identifier` | §6.4.3 |

### CSharpLexer rules

| ANTLR4 Rule | Spec Symbol | Section |
|---|---|---|
| `BYTE_ORDER_MARK` | *(ANTLR-specific)* | |
| `SINGLE_LINE_DOC_COMMENT` | `Single_Line_Comment` | §6.3.3 |
| `EMPTY_DELIMITED_DOC_COMMENT` | `Delimited_Comment` | §6.3.3 |
| `DELIMITED_DOC_COMMENT` | `Delimited_Comment` | §6.3.3 |
| `SINGLE_LINE_COMMENT` | `Single_Line_Comment` | §6.3.3 |
| `DELIMITED_COMMENT` | `Delimited_Comment` | §6.3.3 |
| `WHITESPACES` | `Whitespace` | §6.3.4 |
| `ABSTRACT` … `YIELD` (keyword tokens) | `keyword` / `contextual_keyword` | §6.4.4 |
| `IDENTIFIER` | `Simple_Identifier` | §6.4.3 |
| `LITERAL_ACCESS` | *(ANTLR-specific)* | |
| `INTEGER_LITERAL` | `Decimal_Integer_Literal` | §6.4.5.3 |
| `HEX_INTEGER_LITERAL` | `Hexadecimal_Integer_Literal` | §6.4.5.3 |
| `BIN_INTEGER_LITERAL` | `Binary_Integer_Literal` | §6.4.5.3 |
| `REAL_LITERAL` | `Real_Literal` | §6.4.5.4 |
| `CHARACTER_LITERAL` | `Character_Literal` | §6.4.5.5 |
| `REGULAR_STRING` | `Regular_String_Literal` | §6.4.5.6 |
| `VERBATIUM_STRING` | `Verbatim_String_Literal` | §6.4.5.6 |
| `INTERPOLATED_REGULAR_STRING_START` | `Interpolated_Regular_String_Start` | §12.8.3 |
| `INTERPOLATED_VERBATIUM_STRING_START` | `Interpolated_Verbatim_String_Start` | §12.8.3 |
| `DOUBLE_CURLY_INSIDE` | `Open_Brace_Escape_Sequence` | §12.8.3 |
| `REGULAR_CHAR_INSIDE` | `Interpolated_Regular_String_Element` | §12.8.3 |
| `VERBATIUM_DOUBLE_QUOTE_INSIDE` | `Interpolated_Verbatim_String_Character` | §12.8.3 |
| `DOUBLE_QUOTE_INSIDE` | `Interpolated_Regular_String_End` | §12.8.3 |
| `REGULAR_STRING_INSIDE` | `Interpolated_Regular_String_Element` | §12.8.3 |
| `VERBATIUM_INSIDE_STRING` | `Interpolated_Verbatim_String_Element` | §12.8.3 |
| `DOUBLE_CURLY_CLOSE_INSIDE` | `Close_Brace_Escape_Sequence` | §12.8.3 |
| `FORMAT_STRING` | `Regular_Interpolation_Format` / `Verbatim_Interpolation_Format` | §12.8.3 |
| `OPEN_BRACE` … `OP_RANGE` (punctuator/operator tokens) | `operator_or_punctuator` | §6.4.6 |
| `DIGITS` | `Decimal_Integer_Literal` | §6.4.5.3 |
| `DEFINE` | `PP_Declaration` | §6.5.4 |
| `UNDEF` | `PP_Declaration` | §6.5.4 |
| `ELIF` | `PP_Elif_Section` | §6.5.5 |
| `ENDIF` | `PP_Endif` | §6.5.5 |
| `LINE` | `PP_Line` | §6.5.8 |
| `ERROR` | `PP_Diagnostic` | §6.5.6 |
| `WARNING` | `PP_Diagnostic` | §6.5.6 |
| `REGION` | `PP_Start_Region` | §6.5.7 |
| `ENDREGION` | `PP_End_Region` | §6.5.7 |
| `PRAGMA` | `PP_Pragma` | §6.5.9 |
| `NULLABLE` | *(ANTLR-specific, C# 8 nullable directive)* | |
| `DIRECTIVE_HIDDEN` | `PP_Line_Indicator` | §6.5.8 |
| `CONDITIONAL_SYMBOL` | `PP_Primary_Expression` | §6.5.3 |
| `TEXT` | `PP_Message` / `PP_Pragma_Text` | §6.5.6 / §6.5.9 |
| `SKIPPED_SECTION` | *(ANTLR-specific — emitted by `CSharpLexerBase` for false `#if` blocks)* | |
