CREATE WAREHOUSE IF NOT EXISTS wh1
    WAREHOUSE_SIZE                      = MEDIUM
    WAREHOUSE_TYPE                      = 'SNOWPARK-OPTIMIZED'
    MAX_CLUSTER_COUNT                   = 2
    MIN_CLUSTER_COUNT                   = 1
    SCALING_POLICY                      = ECONOMY
    AUTO_SUSPEND                        = 15
    AUTO_RESUME                         = TRUE
    INITIALLY_SUSPENDED                 = TRUE
    COMMENT                             = 'text'
    ENABLE_QUERY_ACCELERATION           = TRUE
    QUERY_ACCELERATION_MAX_SCALE_FACTOR = 1
    STATEMENT_TIMEOUT_IN_SECONDS        = 10
;

CREATE WAREHOUSE IF NOT EXISTS wh2
    WAREHOUSE_SIZE                      = SMALL
    WAREHOUSE_TYPE                      = STANDARD
    MAX_CLUSTER_COUNT                   = 2
    MIN_CLUSTER_COUNT                   = 1
    SCALING_POLICY                      = ECONOMY
    AUTO_SUSPEND                        = 15
    AUTO_RESUME                         = TRUE
    INITIALLY_SUSPENDED                 = TRUE
    COMMENT                             = 'text'
    ENABLE_QUERY_ACCELERATION           = TRUE
    QUERY_ACCELERATION_MAX_SCALE_FACTOR = 1
    STATEMENT_TIMEOUT_IN_SECONDS        = 10
;

ALTER WAREHOUSE IF EXISTS wh1 SET MAX_CONCURRENCY_LEVEL = 8;

ALTER WAREHOUSE IF EXISTS wh2 SET WAREHOUSE_SIZE = MEDIUM, WAREHOUSE_TYPE = 'SNOWPARK-OPTIMIZED';
