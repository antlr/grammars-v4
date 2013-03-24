//Define a grammar to parse statements from postgresql files
grammar Postgresqlscript;

sqlscript
    : sqlstatement*
      EOF
    | sqlstatement
      (WS sqlstatement)*
      WS*
      EOF
    | EOF
    ;

sqlstatement
    : KEYWORD term+ SEMI
    ;

KEYWORD :
      ABORT_P_
    | ABSOLUTE_P_
    | ACCESS_
    | ACTION_
    | ADD_P_
    | ADMIN_
    | AFTER_
    | AGGREGATE_
    | ALL_
    | ALSO_
    | ALTER_
    | ALWAYS_
    | ANALYSE_
    | ANALYZE_
    | AND_
    | ANY_
    | ARRAY_
    | AS_
    | ASC_
    | ASSERTION_
    | ASSIGNMENT_
    | ASYMMETRIC_
    | AT_
    | AUTHORIZATION_
    | BACKWARD_
    | BEFORE_
    | BEGIN_P_
    | BETWEEN_
    | BIGINT_
    | BINARY_
    | BIT_
    | BOOLEAN_P_
    | BOTH_
    | BY_
    | CACHE_
    | CALLED_
    | CASCADE_
    | CASCADED_
    | CASE_
    | CAST_
    | CATALOG_P_
    | CHAIN_
    | CHAR_P_
    | CHARACTER_
    | CHARACTERISTICS_
    | CHECK_
    | CHECKPOINT_
    | CLASS_
    | CLOSE_
    | CLUSTER_
    | COALESCE_
    | COLLATE_
    | COLUMN_
    | COMMENT_
    | COMMENTS_
    | COMMIT_
    | COMMITTED_
    | CONCURRENTLY_
    | CONFIGURATION_
    | CONNECTION_
    | CONSTRAINT_
    | CONSTRAINTS_
    | CONTENT_P_
    | CONTINUE_P_
    | CONVERSION_P_
    | COPY_
    | COST_
    | CREATE_
    | CREATEDB_
    | CREATEROLE_
    | CREATEUSER_
    | CROSS_
    | CSV_
    | CURRENT_P_
    | CURRENT_CATALOG_
    | CURRENT_DATE_
    | CURRENT_ROLE_
    | CURRENT_SCHEMA_
    | CURRENT_TIME_
    | CURRENT_TIMESTAMP_
    | CURRENT_USER_
    | CURSOR_
    | CYCLE_
    | DATA_P_
    | DATABASE_
    | DAY_P_
    | DEALLOCATE_
    | DEC_
    | DECIMAL_P_
    | DECLARE_
    | DEFAULT_
    | DEFAULTS_
    | DEFERRABLE_
    | DEFERRED_
    | DEFINER_
    | DELETE_P_
    | DELIMITER_
    | DELIMITERS_
    | DESC_
    | DICTIONARY_
    | DISABLE_P_
    | DISCARD_
    | DISTINCT_
    | DO_
    | DOCUMENT_P_
    | DOMAIN_P_
    | DOUBLE_P_
    | DROP_
    | EACH_
    | ELSE_
    | ENABLE_P_
    | ENCODING_
    | ENCRYPTED_
    | END_P_
    | ENUM_P_
    | ESCAPE_
    | EXCEPT_
    | EXCLUDE_
    | EXCLUDING_
    | EXCLUSIVE_
    | EXECUTE_
    | EXISTS_
    | EXPLAIN_
    | EXTERNAL_
    | EXTRACT_
    | FALSE_P_
    | FAMILY_
    | FETCH_
    | FIRST_P_
    | FLOAT_P_
    | FOLLOWING_
    | FOR_
    | FORCE_
    | FOREIGN_
    | FORWARD_
    | FREEZE_
    | FROM_
    | FULL_
    | FUNCTION_
    | FUNCTIONS_
    | GLOBAL_
    | GRANT_
    | GRANTED_
    | GREATEST_
    | GROUP_P_
    | HANDLER_
    | HAVING_
    | HEADER_P_
    | HOLD_
    | HOUR_P_
    | IDENTITY_P_
    | IF_P_
    | ILIKE_
    | IMMEDIATE_
    | IMMUTABLE_
    | IMPLICIT_P_
    | IN_P_
    | INCLUDING_
    | INCREMENT_
    | INDEX_
    | INDEXES_
    | INHERIT_
    | INHERITS_
    | INITIALLY_
    | INLINE_P_
    | INNER_P_
    | INOUT_
    | INPUT_P_
    | INSENSITIVE_
    | INSERT_
    | INSTEAD_
    | INT_P_
    | INTEGER_
    | INTERSECT_
    | INTERVAL_
    | INTO_
    | INVOKER_
    | IS_
    | ISNULL_
    | ISOLATION_
    | JOIN_
    | KEY_
    | LANGUAGE_
    | LARGE_P_
    | LAST_P_
    | LC_COLLATE_P_
    | LC_CTYPE_P_
    | LEADING_
    | LEAST_
    | LEFT_
    | LEVEL_
    | LIKE_
    | LIMIT_
    | LISTEN_
    | LOAD_
    | LOCAL_
    | LOCALTIME_
    | LOCALTIMESTAMP_
    | LOCATION_
    | LOCK_P_
    | LOGIN_P_
    | MAPPING_
    | MATCH_
    | MAXVALUE_
    | MINUTE_P_
    | MINVALUE_
    | MODE_
    | MONTH_P_
    | MOVE_
    | NAME_P_
    | NAMES_
    | NATIONAL_
    | NATURAL_
    | NCHAR_
    | NEXT_
    | NO_
    | NOCREATEDB_
    | NOCREATEROLE_
    | NOCREATEUSER_
    | NOINHERIT_
    | NOLOGIN_P_
    | NONE_
    | NOSUPERUSER_
    | NOT_
    | NOTHING_
    | NOTIFY_
    | NOTNULL_
    | NOWAIT_
    | NULL_P_
    | NULLIF_
    | NULLS_P_
    | NUMERIC_
    | OBJECT_P_
    | OF_
    | OFF_
    | OFFSET_
    | OIDS_
    | ON_
    | ONLY_
    | OPERATOR_
    | OPTION_
    | OPTIONS_
    | OR_
    | ORDER_
    | OUT_P_
    | OUTER_P_
    | OVER_
    | OVERLAPS_
    | OVERLAY_
    | OWNED_
    | OWNER_
    | PARSER_
    | PARTIAL_
    | PARTITION_
    | PASSWORD_
    | PLACING_
    | PLANS_
    | POSITION_
    | PRECEDING_
    | PRECISION_
    | PRESERVE_
    | PREPARE_
    | PREPARED_
    | PRIMARY_
    | PRIOR_
    | PRIVILEGES_
    | PROCEDURAL_
    | PROCEDURE_
    | QUOTE_
    | RANGE_
    | READ_
    | REAL_
    | REASSIGN_
    | RECHECK_
    | RECURSIVE_
    | REFERENCES_
    | REINDEX_
    | RELATIVE_P_
    | RELEASE_
    | RENAME_
    | REPEATABLE_
    | REPLACE_
    | REPLICA_
    | RESET_
    | RESTART_
    | RESTRICT_
    | RETURNING_
    | RETURNS_
    | REVOKE_
    | RIGHT_
    | ROLE_
    | ROLLBACK_
    | ROW_
    | ROWS_
    | RULE_
    | SAVEPOINT_
    | SCHEMA_
    | SCROLL_
    | SEARCH_
    | SECOND_P_
    | SECURITY_
    | SELECT_
    | SEQUENCE_
    | SEQUENCES_
    | SERIALIZABLE_
    | SERVER_
    | SESSION_
    | SESSION_USER_
    | SET_
    | SETOF_
    | SHARE_
    | SHOW_
    | SIMILAR_
    | SIMPLE_
    | SMALLINT_
    | SOME_
    | STABLE_
    | STANDALONE_P_
    | START_
    | STATEMENT_
    | STATISTICS_
    | STDIN_
    | STDOUT_
    | STORAGE_
    | STRICT_P_
    | STRIP_P_
    | SUBSTRING_
    | SUPERUSER_P_
    | SYMMETRIC_
    | SYSID_
    | SYSTEM_P_
    | TABLE_
    | TABLES_
    | TABLESPACE_
    | TEMP_
    | TEMPLATE_
    | TEMPORARY_
    | TEXT_P_
    | THEN_
    | TIME_
    | TIMESTAMP_
    | TO_
    | TRAILING_
    | TRANSACTION_
    | TREAT_
    | TRIGGER_
    | TRIM_
    | TRUE_P_
    | TRUNCATE_
    | TRUSTED_
    | TYPE_P_
    | UNBOUNDED_
    | UNCOMMITTED_
    | UNENCRYPTED_
    | UNION_
    | UNIQUE_
    | UNKNOWN_
    | UNLISTEN_
    | UNTIL_
    | UPDATE_
    | USER_
    | USING_
    | VACUUM_
    | VALID_
    | VALIDATOR_
    | VALUE_P_
    | VALUES_
    | VARCHAR_
    | VARIADIC_
    | VARYING_
    | VERBOSE_
    | VERSION_P_
    | VIEW_
    | VOLATILE_
    | WHEN_
    | WHERE_
    | WHITESPACE_P_
    | WINDOW_
    | WITH_
    | WITHOUT_
    | WORK_
    | WRAPPER_
    | WRITE_
    | XML_P_
    | XMLATTRIBUTES_
    | XMLCONCAT_
    | XMLELEMENT_
    | XMLFOREST_
    | XMLPARSE_
    | XMLPI_
    | XMLROOT_
    | XMLSERIALIZE_
    | YEAR_P_
    | YES_P_
    | ZONE_
        ;


//define keywords

ABORT_P_           :  A_ B_ O_ R_ T_ '_' P_;
ABSOLUTE_P_        :  A_ B_ S_ O_ L_ U_ T_ E_ '_' P_;
ACCESS_            :  A_ C_ C_ E_ S_ S_;
ACTION_            :  A_ C_ T_ I_ O_ N_;
ADD_P_             :  A_ D_ D_ '_' P_;
ADMIN_             :  A_ D_ M_ I_ N_;
AFTER_             :  A_ F_ T_ E_ R_;

AGGREGATE_         :  A_ G_ G_ R_ E_ G_ A_ T_ E_;
ALL_               :  A_ L_ L_;
ALSO_              :  A_ L_ S_ O_;
ALTER_             :  A_ L_ T_ E_ R_;
ALWAYS_            :  A_ L_ W_ A_ Y_ S_;
ANALYSE_           :  A_ N_ A_ L_ Y_ S_ E_;
ANALYZE_           :  A_ N_ A_ L_ Y_ Z_ E_;
AND_               :  A_ N_ D_;
ANY_               :  A_ N_ Y_;
ARRAY_             :  A_ R_ R_ A_ Y_;
AS_                :  A_ S_;
ASC_               :  A_ S_ C_;

ASSERTION_         :  A_ S_ S_ E_ R_ T_ I_ O_ N_;
ASSIGNMENT_        :  A_ S_ S_ I_ G_ N_ M_ E_ N_ T_;
ASYMMETRIC_        :  A_ S_ Y_ M_ M_ E_ T_ R_ I_ C_;
AT_                :  A_ T_;
AUTHORIZATION_     :  A_ U_ T_ H_ O_ R_ I_ Z_ A_ T_ I_ O_ N_;


BACKWARD_          :  B_ A_ C_ K_ W_ A_ R_ D_;
BEFORE_            :  B_ E_ F_ O_ R_ E_;
BEGIN_P_           :  B_ E_ G_ I_ N_ '_' P_;
BETWEEN_           :  B_ E_ T_ W_ E_ E_ N_;
BIGINT_            :  B_ I_ G_ I_ N_ T_;
BINARY_            :  B_ I_ N_ A_ R_ Y_;
BIT_               :  B_ I_ T_;

BOOLEAN_P_         :  B_ O_ O_ L_ E_ A_ N_ '_' P_;
BOTH_              :  B_ O_ T_ H_;
BY_                :  B_ Y_;


CACHE_             :  C_ A_ C_ H_ E_;
CALLED_            :  C_ A_ L_ L_ E_ D_;
CASCADE_           :  C_ A_ S_ C_ A_ D_ E_;
CASCADED_          :  C_ A_ S_ C_ A_ D_ E_ D_;
CASE_              :  C_ A_ S_ E_;
CAST_              :  C_ A_ S_ T_;
CATALOG_P_         :  C_ A_ T_ A_ L_ O_ G_ '_' P_;
CHAIN_             :  C_ H_ A_ I_ N_;
CHAR_P_            :  C_ H_ A_ R_ '_' P_;

CHARACTER_         :  C_ H_ A_ R_ A_ C_ T_ E_ R_;
CHARACTERISTICS_   :  C_ H_ A_ R_ A_ C_ T_ E_ R_ I_ S_ T_ I_ C_ S_;
CHECK_             :  C_ H_ E_ C_ K_;
CHECKPOINT_        :  C_ H_ E_ C_ K_ P_ O_ I_ N_ T_;
CLASS_             :  C_ L_ A_ S_ S_;
CLOSE_             :  C_ L_ O_ S_ E_;

CLUSTER_           :  C_ L_ U_ S_ T_ E_ R_;
COALESCE_          :  C_ O_ A_ L_ E_ S_ C_ E_;
COLLATE_           :  C_ O_ L_ L_ A_ T_ E_;
COLUMN_            :  C_ O_ L_ U_ M_ N_;
COMMENT_           :  C_ O_ M_ M_ E_ N_ T_;
COMMENTS_          :  C_ O_ M_ M_ E_ N_ T_ S_;
COMMIT_            :  C_ O_ M_ M_ I_ T_;

COMMITTED_         :  C_ O_ M_ M_ I_ T_ T_ E_ D_;
CONCURRENTLY_      :  C_ O_ N_ C_ U_ R_ R_ E_ N_ T_ L_ Y_;
CONFIGURATION_     :  C_ O_ N_ F_ I_ G_ U_ R_ A_ T_ I_ O_ N_;
CONNECTION_        :  C_ O_ N_ N_ E_ C_ T_ I_ O_ N_;
CONSTRAINT_        :  C_ O_ N_ S_ T_ R_ A_ I_ N_ T_;
CONSTRAINTS_       :  C_ O_ N_ S_ T_ R_ A_ I_ N_ T_ S_;

CONTENT_P_         :  C_ O_ N_ T_ E_ N_ T_ '_' P_;
CONTINUE_P_        :  C_ O_ N_ T_ I_ N_ U_ E_ '_' P_;
CONVERSION_P_      :  C_ O_ N_ V_ E_ R_ S_ I_ O_ N_ '_' P_;
COPY_              :  C_ O_ P_ Y_;
COST_              :  C_ O_ S_ T_;
CREATE_            :  C_ R_ E_ A_ T_ E_;
CREATEDB_          :  C_ R_ E_ A_ T_ E_ D_ B_;

CREATEROLE_        :  C_ R_ E_ A_ T_ E_ R_ O_ L_ E_;
CREATEUSER_        :  C_ R_ E_ A_ T_ E_ U_ S_ E_ R_;
CROSS_             :  C_ R_ O_ S_ S_;
CSV_               :  C_ S_ V_;
CURRENT_P_         :  C_ U_ R_ R_ E_ N_ T_ '_' P_;

CURRENT_CATALOG_   :  C_ U_ R_ R_ E_ N_ T_ '_' C_ A_ T_ A_ L_ O_ G_;
CURRENT_DATE_      :  C_ U_ R_ R_ E_ N_ T_ '_' D_ A_ T_ E_;
CURRENT_ROLE_      :  C_ U_ R_ R_ E_ N_ T_ '_' R_ O_ L_ E_;
CURRENT_SCHEMA_    :  C_ U_ R_ R_ E_ N_ T_ '_' S_ C_ H_ E_ M_ A_;

CURRENT_TIME_      :  C_ U_ R_ R_ E_ N_ T_ '_' T_ I_ M_ E_;
CURRENT_TIMESTAMP_ :  C_ U_ R_ R_ E_ N_ T_ '_' T_ I_ M_ E_ S_ T_ A_ M_ P_;
CURRENT_USER_      :  C_ U_ R_ R_ E_ N_ T_ '_' U_ S_ E_ R_;
CURSOR_            :  C_ U_ R_ S_ O_ R_;
CYCLE_             :  C_ Y_ C_ L_ E_;


DATA_P_            :  D_ A_ T_ A_ '_' P_;
DATABASE_          :  D_ A_ T_ A_ B_ A_ S_ E_;
DAY_P_             :  D_ A_ Y_ '_' P_;
DEALLOCATE_        :  D_ E_ A_ L_ L_ O_ C_ A_ T_ E_;
DEC_               :  D_ E_ C_;
DECIMAL_P_         :  D_ E_ C_ I_ M_ A_ L_ '_' P_;
DECLARE_           :  D_ E_ C_ L_ A_ R_ E_;
DEFAULT_           :  D_ E_ F_ A_ U_ L_ T_;
DEFAULTS_          :  D_ E_ F_ A_ U_ L_ T_ S_;

DEFERRABLE_        :  D_ E_ F_ E_ R_ R_ A_ B_ L_ E_;
DEFERRED_          :  D_ E_ F_ E_ R_ R_ E_ D_;
DEFINER_           :  D_ E_ F_ I_ N_ E_ R_;
DELETE_P_          :  D_ E_ L_ E_ T_ E_ '_' P_;
DELIMITER_         :  D_ E_ L_ I_ M_ I_ T_ E_ R_;
DELIMITERS_        :  D_ E_ L_ I_ M_ I_ T_ E_ R_ S_;
DESC_              :  D_ E_ S_ C_;

DICTIONARY_        :  D_ I_ C_ T_ I_ O_ N_ A_ R_ Y_;
DISABLE_P_         :  D_ I_ S_ A_ B_ L_ E_ '_' P_;
DISCARD_           :  D_ I_ S_ C_ A_ R_ D_;
DISTINCT_          :  D_ I_ S_ T_ I_ N_ C_ T_;
DO_                :  D_ O_;
DOCUMENT_P_        :  D_ O_ C_ U_ M_ E_ N_ T_ '_' P_;
DOMAIN_P_          :  D_ O_ M_ A_ I_ N_ '_' P_;
DOUBLE_P_          :  D_ O_ U_ B_ L_ E_ '_' P_;
DROP_              :  D_ R_ O_ P_;


EACH_              :  E_ A_ C_ H_;
ELSE_              :  E_ L_ S_ E_;
ENABLE_P_          :  E_ N_ A_ B_ L_ E_ '_' P_;
ENCODING_          :  E_ N_ C_ O_ D_ I_ N_ G_;
ENCRYPTED_         :  E_ N_ C_ R_ Y_ P_ T_ E_ D_;
END_P_             :  E_ N_ D_ '_' P_;
ENUM_P_            :  E_ N_ U_ M_ '_' P_;
ESCAPE_            :  E_ S_ C_ A_ P_ E_;
EXCEPT_            :  E_ X_ C_ E_ P_ T_;

EXCLUDE_           :  E_ X_ C_ L_ U_ D_ E_;
EXCLUDING_         :  E_ X_ C_ L_ U_ D_ I_ N_ G_;
EXCLUSIVE_         :  E_ X_ C_ L_ U_ S_ I_ V_ E_;
EXECUTE_           :  E_ X_ E_ C_ U_ T_ E_;
EXISTS_            :  E_ X_ I_ S_ T_ S_;
EXPLAIN_           :  E_ X_ P_ L_ A_ I_ N_;
EXTERNAL_          :  E_ X_ T_ E_ R_ N_ A_ L_;
EXTRACT_           :  E_ X_ T_ R_ A_ C_ T_;


FALSE_P_           :  F_ A_ L_ S_ E_ '_' P_;
FAMILY_            :  F_ A_ M_ I_ L_ Y_;
FETCH_             :  F_ E_ T_ C_ H_;
FIRST_P_           :  F_ I_ R_ S_ T_ '_' P_;
FLOAT_P_           :  F_ L_ O_ A_ T_ '_' P_;
FOLLOWING_         :  F_ O_ L_ L_ O_ W_ I_ N_ G_;
FOR_               :  F_ O_ R_;
FORCE_             :  F_ O_ R_ C_ E_;
FOREIGN_           :  F_ O_ R_ E_ I_ G_ N_;
FORWARD_           :  F_ O_ R_ W_ A_ R_ D_;

FREEZE_            :  F_ R_ E_ E_ Z_ E_;
FROM_              :  F_ R_ O_ M_;
FULL_              :  F_ U_ L_ L_;
FUNCTION_          :  F_ U_ N_ C_ T_ I_ O_ N_;
FUNCTIONS_         :  F_ U_ N_ C_ T_ I_ O_ N_ S_;


GLOBAL_            :  G_ L_ O_ B_ A_ L_;
GRANT_             :  G_ R_ A_ N_ T_;
GRANTED_           :  G_ R_ A_ N_ T_ E_ D_;
GREATEST_          :  G_ R_ E_ A_ T_ E_ S_ T_;
GROUP_P_           :  G_ R_ O_ U_ P_ '_' P_;


HANDLER_           :  H_ A_ N_ D_ L_ E_ R_;
HAVING_            :  H_ A_ V_ I_ N_ G_;
HEADER_P_          :  H_ E_ A_ D_ E_ R_ '_' P_;
HOLD_              :  H_ O_ L_ D_;
HOUR_P_            :  H_ O_ U_ R_ '_' P_;


IDENTITY_P_        :  I_ D_ E_ N_ T_ I_ T_ Y_ '_' P_;
IF_P_              :  I_ F_ '_' P_;
ILIKE_             :  I_ L_ I_ K_ E_;
IMMEDIATE_         :  I_ M_ M_ E_ D_ I_ A_ T_ E_;
IMMUTABLE_         :  I_ M_ M_ U_ T_ A_ B_ L_ E_;
IMPLICIT_P_        :  I_ M_ P_ L_ I_ C_ I_ T_ '_' P_;
IN_P_              :  I_ N_ '_' P_;

INCLUDING_         :  I_ N_ C_ L_ U_ D_ I_ N_ G_;
INCREMENT_         :  I_ N_ C_ R_ E_ M_ E_ N_ T_;
INDEX_             :  I_ N_ D_ E_ X_;
INDEXES_           :  I_ N_ D_ E_ X_ E_ S_;
INHERIT_           :  I_ N_ H_ E_ R_ I_ T_;
INHERITS_          :  I_ N_ H_ E_ R_ I_ T_ S_;
INITIALLY_         :  I_ N_ I_ T_ I_ A_ L_ L_ Y_;
INLINE_P_          :  I_ N_ L_ I_ N_ E_ '_' P_;

INNER_P_           :  I_ N_ N_ E_ R_ '_' P_;
INOUT_             :  I_ N_ O_ U_ T_;
INPUT_P_           :  I_ N_ P_ U_ T_ '_' P_;
INSENSITIVE_       :  I_ N_ S_ E_ N_ S_ I_ T_ I_ V_ E_;
INSERT_            :  I_ N_ S_ E_ R_ T_;
INSTEAD_           :  I_ N_ S_ T_ E_ A_ D_;
INT_P_             :  I_ N_ T_ '_' P_;
INTEGER_           :  I_ N_ T_ E_ G_ E_ R_;

INTERSECT_         :  I_ N_ T_ E_ R_ S_ E_ C_ T_;
INTERVAL_          :  I_ N_ T_ E_ R_ V_ A_ L_;
INTO_              :  I_ N_ T_ O_;
INVOKER_           :  I_ N_ V_ O_ K_ E_ R_;
IS_                :  I_ S_;
ISNULL_            :  I_ S_ N_ U_ L_ L_;
ISOLATION_         :  I_ S_ O_ L_ A_ T_ I_ O_ N_;


JOIN_              :  J_ O_ I_ N_;


KEY_               :  K_ E_ Y_;


LANGUAGE_          :  L_ A_ N_ G_ U_ A_ G_ E_;
LARGE_P_           :  L_ A_ R_ G_ E_ '_' P_;
LAST_P_            :  L_ A_ S_ T_ '_' P_;
LC_COLLATE_P_      :  L_ C_ '_' C_ O_ L_ L_ A_ T_ E_ '_' P_;
LC_CTYPE_P_        :  L_ C_ '_' C_ T_ Y_ P_ E_ '_' P_;
LEADING_           :  L_ E_ A_ D_ I_ N_ G_;

LEAST_             :  L_ E_ A_ S_ T_;
LEFT_              :  L_ E_ F_ T_;
LEVEL_             :  L_ E_ V_ E_ L_;
LIKE_              :  L_ I_ K_ E_;
LIMIT_             :  L_ I_ M_ I_ T_;
LISTEN_            :  L_ I_ S_ T_ E_ N_;
LOAD_              :  L_ O_ A_ D_;
LOCAL_             :  L_ O_ C_ A_ L_;
LOCALTIME_         :  L_ O_ C_ A_ L_ T_ I_ M_ E_;
LOCALTIMESTAMP_    :  L_ O_ C_ A_ L_ T_ I_ M_ E_ S_ T_ A_ M_ P_;

LOCATION_          :  L_ O_ C_ A_ T_ I_ O_ N_;
LOCK_P_            :  L_ O_ C_ K_ '_' P_;
LOGIN_P_           :  L_ O_ G_ I_ N_ '_' P_;


MAPPING_           :  M_ A_ P_ P_ I_ N_ G_;
MATCH_             :  M_ A_ T_ C_ H_;
MAXVALUE_          :  M_ A_ X_ V_ A_ L_ U_ E_;
MINUTE_P_          :  M_ I_ N_ U_ T_ E_ '_' P_;
MINVALUE_          :  M_ I_ N_ V_ A_ L_ U_ E_;
MODE_              :  M_ O_ D_ E_;
MONTH_P_           :  M_ O_ N_ T_ H_ '_' P_;
MOVE_              :  M_ O_ V_ E_;


NAME_P_            :  N_ A_ M_ E_ '_' P_;
NAMES_             :  N_ A_ M_ E_ S_;
NATIONAL_          :  N_ A_ T_ I_ O_ N_ A_ L_;
NATURAL_           :  N_ A_ T_ U_ R_ A_ L_;
NCHAR_             :  N_ C_ H_ A_ R_;
NEXT_              :  N_ E_ X_ T_;
NO_                :  N_ O_;
NOCREATEDB_        :  N_ O_ C_ R_ E_ A_ T_ E_ D_ B_;

NOCREATEROLE_      :  N_ O_ C_ R_ E_ A_ T_ E_ R_ O_ L_ E_;
NOCREATEUSER_      :  N_ O_ C_ R_ E_ A_ T_ E_ U_ S_ E_ R_;
NOINHERIT_         :  N_ O_ I_ N_ H_ E_ R_ I_ T_;
NOLOGIN_P_         :  N_ O_ L_ O_ G_ I_ N_ '_' P_;
NONE_              :  N_ O_ N_ E_;
NOSUPERUSER_       :  N_ O_ S_ U_ P_ E_ R_ U_ S_ E_ R_;

NOT_               :  N_ O_ T_;
NOTHING_           :  N_ O_ T_ H_ I_ N_ G_;
NOTIFY_            :  N_ O_ T_ I_ F_ Y_;
NOTNULL_           :  N_ O_ T_ N_ U_ L_ L_;
NOWAIT_            :  N_ O_ W_ A_ I_ T_;
NULL_P_            :  N_ U_ L_ L_ '_' P_;
NULLIF_            :  N_ U_ L_ L_ I_ F_;
NULLS_P_           :  N_ U_ L_ L_ S_ '_' P_;
NUMERIC_           :  N_ U_ M_ E_ R_ I_ C_;


OBJECT_P_          :  O_ B_ J_ E_ C_ T_ '_' P_;
OF_                :  O_ F_;
OFF_               :  O_ F_ F_;
OFFSET_            :  O_ F_ F_ S_ E_ T_;
OIDS_              :  O_ I_ D_ S_;
ON_                :  O_ N_;
ONLY_              :  O_ N_ L_ Y_;
OPERATOR_          :  O_ P_ E_ R_ A_ T_ O_ R_;
OPTION_            :  O_ P_ T_ I_ O_ N_;
OPTIONS_           :  O_ P_ T_ I_ O_ N_ S_;
OR_                :  O_ R_;

ORDER_             :  O_ R_ D_ E_ R_;
OUT_P_             :  O_ U_ T_ '_' P_;
OUTER_P_           :  O_ U_ T_ E_ R_ '_' P_;
OVER_              :  O_ V_ E_ R_;
OVERLAPS_          :  O_ V_ E_ R_ L_ A_ P_ S_;
OVERLAY_           :  O_ V_ E_ R_ L_ A_ Y_;
OWNED_             :  O_ W_ N_ E_ D_;
OWNER_             :  O_ W_ N_ E_ R_;


PARSER_            :  P_ A_ R_ S_ E_ R_;
PARTIAL_           :  P_ A_ R_ T_ I_ A_ L_;
PARTITION_         :  P_ A_ R_ T_ I_ T_ I_ O_ N_;
PASSWORD_          :  P_ A_ S_ S_ W_ O_ R_ D_;
PLACING_           :  P_ L_ A_ C_ I_ N_ G_;
PLANS_             :  P_ L_ A_ N_ S_;
POSITION_          :  P_ O_ S_ I_ T_ I_ O_ N_;

PRECEDING_         :  P_ R_ E_ C_ E_ D_ I_ N_ G_;
PRECISION_         :  P_ R_ E_ C_ I_ S_ I_ O_ N_;
PRESERVE_          :  P_ R_ E_ S_ E_ R_ V_ E_;
PREPARE_           :  P_ R_ E_ P_ A_ R_ E_;
PREPARED_          :  P_ R_ E_ P_ A_ R_ E_ D_;
PRIMARY_           :  P_ R_ I_ M_ A_ R_ Y_;

PRIOR_             :  P_ R_ I_ O_ R_;
PRIVILEGES_        :  P_ R_ I_ V_ I_ L_ E_ G_ E_ S_;
PROCEDURAL_        :  P_ R_ O_ C_ E_ D_ U_ R_ A_ L_;
PROCEDURE_         :  P_ R_ O_ C_ E_ D_ U_ R_ E_;


QUOTE_             :  Q_ U_ O_ T_ E_;


RANGE_             :  R_ A_ N_ G_ E_;
READ_              :  R_ E_ A_ D_;
REAL_              :  R_ E_ A_ L_;
REASSIGN_          :  R_ E_ A_ S_ S_ I_ G_ N_;
RECHECK_           :  R_ E_ C_ H_ E_ C_ K_;
RECURSIVE_         :  R_ E_ C_ U_ R_ S_ I_ V_ E_;
REFERENCES_        :  R_ E_ F_ E_ R_ E_ N_ C_ E_ S_;
REINDEX_           :  R_ E_ I_ N_ D_ E_ X_;

RELATIVE_P_        :  R_ E_ L_ A_ T_ I_ V_ E_ '_' P_;
RELEASE_           :  R_ E_ L_ E_ A_ S_ E_;
RENAME_            :  R_ E_ N_ A_ M_ E_;
REPEATABLE_        :  R_ E_ P_ E_ A_ T_ A_ B_ L_ E_;
REPLACE_           :  R_ E_ P_ L_ A_ C_ E_;
REPLICA_           :  R_ E_ P_ L_ I_ C_ A_;
RESET_             :  R_ E_ S_ E_ T_;
RESTART_           :  R_ E_ S_ T_ A_ R_ T_;

RESTRICT_          :  R_ E_ S_ T_ R_ I_ C_ T_;
RETURNING_         :  R_ E_ T_ U_ R_ N_ I_ N_ G_;
RETURNS_           :  R_ E_ T_ U_ R_ N_ S_;
REVOKE_            :  R_ E_ V_ O_ K_ E_;
RIGHT_             :  R_ I_ G_ H_ T_;
ROLE_              :  R_ O_ L_ E_;
ROLLBACK_          :  R_ O_ L_ L_ B_ A_ C_ K_;
ROW_               :  R_ O_ W_;
ROWS_              :  R_ O_ W_ S_;
RULE_              :  R_ U_ L_ E_;


SAVEPOINT_         :  S_ A_ V_ E_ P_ O_ I_ N_ T_;
SCHEMA_            :  S_ C_ H_ E_ M_ A_;
SCROLL_            :  S_ C_ R_ O_ L_ L_;
SEARCH_            :  S_ E_ A_ R_ C_ H_;
SECOND_P_          :  S_ E_ C_ O_ N_ D_ '_' P_;
SECURITY_          :  S_ E_ C_ U_ R_ I_ T_ Y_;
SELECT_            :  S_ E_ L_ E_ C_ T_;
SEQUENCE_          :  S_ E_ Q_ U_ E_ N_ C_ E_;
SEQUENCES_         :  S_ E_ Q_ U_ E_ N_ C_ E_ S_;

SERIALIZABLE_      :  S_ E_ R_ I_ A_ L_ I_ Z_ A_ B_ L_ E_;
SERVER_            :  S_ E_ R_ V_ E_ R_;
SESSION_           :  S_ E_ S_ S_ I_ O_ N_;
SESSION_USER_      :  S_ E_ S_ S_ I_ O_ N_ '_' U_ S_ E_ R_;
SET_               :  S_ E_ T_;
SETOF_             :  S_ E_ T_ O_ F_;
SHARE_             :  S_ H_ A_ R_ E_;

SHOW_              :  S_ H_ O_ W_;
SIMILAR_           :  S_ I_ M_ I_ L_ A_ R_;
SIMPLE_            :  S_ I_ M_ P_ L_ E_;
SMALLINT_          :  S_ M_ A_ L_ L_ I_ N_ T_;
SOME_              :  S_ O_ M_ E_;
STABLE_            :  S_ T_ A_ B_ L_ E_;
STANDALONE_P_      :  S_ T_ A_ N_ D_ A_ L_ O_ N_ E_ '_' P_;
START_             :  S_ T_ A_ R_ T_;
STATEMENT_         :  S_ T_ A_ T_ E_ M_ E_ N_ T_;

STATISTICS_        :  S_ T_ A_ T_ I_ S_ T_ I_ C_ S_;
STDIN_             :  S_ T_ D_ I_ N_;
STDOUT_            :  S_ T_ D_ O_ U_ T_;
STORAGE_           :  S_ T_ O_ R_ A_ G_ E_;
STRICT_P_          :  S_ T_ R_ I_ C_ T_ '_' P_;
STRIP_P_           :  S_ T_ R_ I_ P_ '_' P_;
SUBSTRING_         :  S_ U_ B_ S_ T_ R_ I_ N_ G_;
SUPERUSER_P_       :  S_ U_ P_ E_ R_ U_ S_ E_ R_ '_' P_;

SYMMETRIC_         :  S_ Y_ M_ M_ E_ T_ R_ I_ C_;
SYSID_             :  S_ Y_ S_ I_ D_;
SYSTEM_P_          :  S_ Y_ S_ T_ E_ M_ '_' P_;


TABLE_             :  T_ A_ B_ L_ E_;
TABLES_            :  T_ A_ B_ L_ E_ S_;
TABLESPACE_        :  T_ A_ B_ L_ E_ S_ P_ A_ C_ E_;
TEMP_              :  T_ E_ M_ P_;
TEMPLATE_          :  T_ E_ M_ P_ L_ A_ T_ E_;
TEMPORARY_         :  T_ E_ M_ P_ O_ R_ A_ R_ Y_;
TEXT_P_            :  T_ E_ X_ T_ '_' P_;
THEN_              :  T_ H_ E_ N_;
TIME_              :  T_ I_ M_ E_;
TIMESTAMP_         :  T_ I_ M_ E_ S_ T_ A_ M_ P_;

TO_                :  T_ O_;
TRAILING_          :  T_ R_ A_ I_ L_ I_ N_ G_;
TRANSACTION_       :  T_ R_ A_ N_ S_ A_ C_ T_ I_ O_ N_;
TREAT_             :  T_ R_ E_ A_ T_;
TRIGGER_           :  T_ R_ I_ G_ G_ E_ R_;
TRIM_              :  T_ R_ I_ M_;
TRUE_P_            :  T_ R_ U_ E_ '_' P_;

TRUNCATE_          :  T_ R_ U_ N_ C_ A_ T_ E_;
TRUSTED_           :  T_ R_ U_ S_ T_ E_ D_;
TYPE_P_            :  T_ Y_ P_ E_ '_' P_;


UNBOUNDED_         :  U_ N_ B_ O_ U_ N_ D_ E_ D_;
UNCOMMITTED_       :  U_ N_ C_ O_ M_ M_ I_ T_ T_ E_ D_;
UNENCRYPTED_       :  U_ N_ E_ N_ C_ R_ Y_ P_ T_ E_ D_;
UNION_             :  U_ N_ I_ O_ N_;
UNIQUE_            :  U_ N_ I_ Q_ U_ E_;
UNKNOWN_           :  U_ N_ K_ N_ O_ W_ N_;
UNLISTEN_          :  U_ N_ L_ I_ S_ T_ E_ N_;
UNTIL_             :  U_ N_ T_ I_ L_;

UPDATE_            :  U_ P_ D_ A_ T_ E_;
USER_              :  U_ S_ E_ R_;
USING_             :  U_ S_ I_ N_ G_;


VACUUM_            :  V_ A_ C_ U_ U_ M_;
VALID_             :  V_ A_ L_ I_ D_;
VALIDATOR_         :  V_ A_ L_ I_ D_ A_ T_ O_ R_;
VALUE_P_           :  V_ A_ L_ U_ E_ '_' P_;
VALUES_            :  V_ A_ L_ U_ E_ S_;
VARCHAR_           :  V_ A_ R_ C_ H_ A_ R_;
VARIADIC_          :  V_ A_ R_ I_ A_ D_ I_ C_;
VARYING_           :  V_ A_ R_ Y_ I_ N_ G_;

VERBOSE_           :  V_ E_ R_ B_ O_ S_ E_;
VERSION_P_         :  V_ E_ R_ S_ I_ O_ N_ '_' P_;
VIEW_              :  V_ I_ E_ W_;
VOLATILE_          :  V_ O_ L_ A_ T_ I_ L_ E_;


WHEN_              :  W_ H_ E_ N_;
WHERE_             :  W_ H_ E_ R_ E_;
WHITESPACE_P_      :  W_ H_ I_ T_ E_ S_ P_ A_ C_ E_ '_' P_;
WINDOW_            :  W_ I_ N_ D_ O_ W_;
WITH_              :  W_ I_ T_ H_;
WITHOUT_           :  W_ I_ T_ H_ O_ U_ T_;
WORK_              :  W_ O_ R_ K_;
WRAPPER_           :  W_ R_ A_ P_ P_ E_ R_;
WRITE_             :  W_ R_ I_ T_ E_;


XML_P_             :  X_ M_ L_ '_' P_;
XMLATTRIBUTES_     :  X_ M_ L_ A_ T_ T_ R_ I_ B_ U_ T_ E_ S_;
XMLCONCAT_         :  X_ M_ L_ C_ O_ N_ C_ A_ T_;
XMLELEMENT_        :  X_ M_ L_ E_ L_ E_ M_ E_ N_ T_;
XMLFOREST_         :  X_ M_ L_ F_ O_ R_ E_ S_ T_;
XMLPARSE_          :  X_ M_ L_ P_ A_ R_ S_ E_;

XMLPI_             :  X_ M_ L_ P_ I_;
XMLROOT_           :  X_ M_ L_ R_ O_ O_ T_;
XMLSERIALIZE_      :  X_ M_ L_ S_ E_ R_ I_ A_ L_ I_ Z_ E_;


YEAR_P_            :  Y_ E_ A_ R_ '_' P_;
YES_P_             :  Y_ E_ S_ '_' P_;


ZONE_              :  Z_ O_ N_ E_;


// Other symbols
// basic token definition ------------------------------------------------------------

DIVIDE	: (  D_ I_ V_ ) | '/' ;
MOD_SYM	: (  M_ O_ D_ ) | '%' ;
OR_SYM	: (  O_ R_ ) | '||';
AND_SYM	: (  A_ N_ D_ ) | '&&';

ARROW	: '=>' ;
EQ_SYM	: '=' | '<=>' ;
NOT_EQ	: '<>' | '!=' | '~='| '^=';
LET	: '<=' ;
GET	: '>=' ;
SET_VAR	: ':=' ;
SHIFT_LEFT	: '<<' ;
SHIFT_RIGHT	: '>>' ;
ALL_FIELDS	: '.*' ;

SEMI	: ';' ;
COLON	: ':' ;
DOT	: '.' ;
COMMA	: ',' ;
ASTERISK: '*' ;
RPAREN	: ')' ;
LPAREN	: '(' ;
RBRACK	: ']' ;
LBRACK	: '[' ;
PLUS	: '+' ;
MINUS	: '-' ;
NEGATION: '~' ;
VERTBAR	: '|' ;
BITAND	: '&' ;
POWER_OP: '^' ;
GTH	: '>' ;
LTH	: '<' ;


// Define terms

term : ID
     | KEYWORD
     | operator
     | CharacterLiteral
     ;

ID : [A-Za-z0-9_]+;

CharacterLiteral
    :   '\'' (  ~('\''|'\\') )* '\''
    ;

operator : DIVIDE
|MOD_SYM
|OR_SYM
|AND_SYM
|ARROW
|EQ_SYM
|NOT_EQ
|LET
|GET
|SET_VAR
|SHIFT_LEFT
|SHIFT_RIGHT
|ALL_FIELDS
|COLON
|DOT
|COMMA
|ASTERISK
|RPAREN
|LPAREN
|RBRACK
|LBRACK
|PLUS
|MINUS
|NEGATION
|VERTBAR
|BITAND
|POWER_OP
|GTH
|LTH;


/* 
   Define letters to get case insensitivity, can also do this by redefining the
   input stream
*/

fragment A_ : ('a'|'A');
fragment B_ : ('b'|'B');
fragment C_ : ('c'|'C');
fragment D_ : ('d'|'D');
fragment E_ : ('e'|'E');
fragment F_ : ('f'|'F');
fragment G_ : ('g'|'G');
fragment H_ : ('h'|'H');
fragment I_ : ('i'|'I');
fragment J_ : ('j'|'J');
fragment K_ : ('k'|'K');
fragment L_ : ('l'|'L');
fragment M_ : ('m'|'M');
fragment N_ : ('n'|'N');
fragment O_ : ('o'|'O');
fragment P_ : ('p'|'P');
fragment Q_ : ('q'|'Q');
fragment R_ : ('r'|'R');
fragment S_ : ('s'|'S');
fragment T_ : ('t'|'T');
fragment U_ : ('u'|'U');
fragment V_ : ('v'|'V');
fragment W_ : ('w'|'W');
fragment X_ : ('x'|'X');
fragment Y_ : ('y'|'Y');
fragment Z_ : ('z'|'Z');

LINE_COMMENT : '--' ~[\r\n]* '\r'? '\n' -> channel(HIDDEN) ; // skip sql comment

WS : [ \t\r\n]+ -> channel(HIDDEN) ; // skip spaces, tabs, newlines
