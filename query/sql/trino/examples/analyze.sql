ANALYZE foo;
ANALYZE foo WITH ( "string" = 'bar', "long" = 42, computed = concat('ban', 'ana'), a = ARRAY[ 'v1', 'v2' ] );
EXPLAIN ANALYZE foo;
EXPLAIN ANALYZE ANALYZE foo;
