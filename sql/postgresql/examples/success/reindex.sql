REINDEX SCHEMA CONCURRENTLY :temp_schema_name;

SELECT pg_my_temp_schema()::regnamespace as temp_schema_name \gset
