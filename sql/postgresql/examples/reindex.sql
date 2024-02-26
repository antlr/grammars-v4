REINDEX SCHEMA CONCURRENTLY :temp_schema_name;

SELECT pg_my_temp_schema()::regnamespace as :temp_schema_name;

REINDEX (CONCURRENTLY) TABLE concur_reindex_tab; -- notice

REINDEX (TABLESPACE, VERBOSE) SYSTEM concur_reindex_tab;

REINDEX (CONCURRENTLY, TABLESPACE, VERBOSE) SYSTEM concur_reindex_tab;

REINDEX (VERBOSE) TABLE :temp_schema_name;

REINDEX (VERBOSE) DATABASE :temp_schema_name;

REINDEX (VERBOSE) SYSTEM :temp_schema_name;