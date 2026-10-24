SET $wh_name = 'test';
SET $size = 'XSMALL';

CREATE OR REPLACE WAREHOUSE IDENTIFIER($wh_name)
    WAREHOUSE_SIZE                      = $size
    MAX_CLUSTER_COUNT                   = 1
    MIN_CLUSTER_COUNT                   = 1
    SCALING_POLICY                      = ECONOMY
    AUTO_SUSPEND                        = 15
    AUTO_RESUME                         = TRUE
    INITIALLY_SUSPENDED                 = TRUE
    COMMENT                             = ''
    ENABLE_QUERY_ACCELERATION           = TRUE
	QUERY_ACCELERATION_MAX_SCALE_FACTOR = 1
	STATEMENT_TIMEOUT_IN_SECONDS        = 1
;

USE WAREHOUSE IDENTIFIER($wh_name);

DROP WAREHOUSE IDENTIFIER($wh_name);
