SELECT pg_my_temp_schema()::regnamespace as temp_schema_name \gset
REINDEX SCHEMA CONCURRENTLY :temp_schema_name;

