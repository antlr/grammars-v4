-- Creating a UDF C Function
CREATE FUNCTION find_text (
    searched_string VARCHAR(500),
    pattern VARCHAR(500))
RETURNS CHARACTER
LANGUAGE C
NO SQL
EXTERNAL
PARAMETER STYLE SQL;

-- write_mq C function
CREATE FUNCTION write_mq(
    qmgr VARCHAR(256),
    qnm VARCHAR(256),
    channel VARCHAR(256),
    vcmsg VARCHAR(32000))
RETURNS INTEGER
LANGUAGE C
NO SQL
PARAMETER STYLE SQL
EXTERNAL NAME 'F:emruwmq:SI:cmqc:/usr/include/cmqc.h:SL:mqic:SL:mqmcs:SS:emruwmq:/home/user/emruwmq/emruwmq.c';

-- Creating an EXTERNAL SECURITY Clause in a UDF
CREATE FUNCTION sales_collect (
    store_number INTEGER,
    item_no INTEGER)
    RETURNS INTEGER
LANGUAGE C
NO SQL
EXTERNAL NAME 'cs!salecol!salecollector.c'
PARAMETER STYLE SQL
EXTERNAL SECURITY DEFINER sales;

-- UDT Input Parameter Data Type in Scalar UDF Definition
CREATE FUNCTION udf_agch002002udt (
    parameter_1 varchar_udt)
    RETURNS varchar_udt
CLASS AGGREGATE (20000)
LANGUAGE C
NO SQL
EXTERNAL NAME 'CS!udf_agch002002udt!udf_agch002002udt.c'
PARAMETER STYLE SQL;

-- Creating Functions that Algorithmically Compress LOB- Related Data
CREATE FUNCTION clob_compress (a CLOB AS LOCATOR)
RETURNS BLOB AS LOCATOR
SPECIFIC clob_compress
LANGUAGE C
NO SQL
FOR COMPRESS
PARAMETER STYLE TD_GENERAL
RETURNS NULL ON NULL INPUT
DETERMINISTIC
EXTERNAL NAME 'cs!clobcompress!clobcompress.c';

CREATE FUNCTION d_clob_compress (a DCLOB)
RETURNS BLOB AS LOCATOR
SPECIFIC d_clob_compress
LANGUAGE C
NO SQL
FOR COMPRESS
PARAMETER STYLE TD_GENERAL RETURNS NULL ON NULL INPUT
DETERMINISTIC
EXTERNAL NAME 'cs!dclobcompress!dclobcompress.c';

-- Creating Functions that Decompress Algorithmically Compressed LOB-Related Data
CREATE FUNCTION clob_decompress (a BLOB AS LOCATOR)
RETURNS CLOB AS LOCATOR
SPECIFIC clob_decompress
LANGUAGE C
NO SQL
FOR DECOMPRESS
PARAMETER STYLE TD_GENERAL
RETURNS NULL ON NULL INPUT
DETERMINISTIC
EXTERNAL NAME 'cs!clobdecompress!clobdecompress.c';

CREATE FUNCTION d_clob_decompress (a BLOB AS LOCATOR)
RETURNS DCLOB
SPECIFIC d_clob_decompress
LANGUAGE C
NO SQL
FOR DECOMPRESS
PARAMETER STYLE TD_GENERAL RETURNS NULL ON NULL INPUT
DETERMINISTIC
EXTERNAL NAME 'cs!dclobdecompress!dclobdecompress.c';

CREATE FUNCTION s_clob_decompress (a BLOB AS LOCATOR)
RETURNS SCLOB
SPECIFIC s_clob_decompress
LANGUAGE C
NO SQL
FOR DECOMPRESS
PARAMETER STYLE TD_GENERAL RETURNS NULL ON NULL INPUT
DETERMINISTIC
EXTERNAL NAME 'cs!sclobdecompress!sclobdecompress.c';

-- Creating and Using a VARIANT_TYPE Input Parameter UDT Data Type in a UDF Definition
CREATE FUNCTION udf_variant (parameter_1 VARIANT_TYPE)
RETURNS integer_udt CLASS AGGREGATE(4)
LANGUAGE C
NO SQL
EXTERNAL NAME 'CS!udf_variant!udf_variant.c'
PARAMETER STYLE SQL;

-- Using the TD_ANYTYPE Parameter and RETURNS Clause Data Type in a UDF Definition
CREATE FUNCTION ascii (
string_expr TD_ANYTYPE)
RETURNS TD_ANYTYPE
LANGUAGE C
NO SQL
SPECIFIC ascii
EXTERNAL NAME 'CS!ascii!ascii.c'
PARAMETER STYLE SQL;

-- Creating a Java Scalar Function
CREATE FUNCTION judf_mybigint (
    par1 BIGINT)
RETURNS BIGINT
CAST FROM INTEGER
SPECIFIC judf_mybigint
LANGUAGE JAVA
NO SQL
PARAMETER STYLE JAVA
NOT DETERMINISTIC
CALLED ON NULL INPUT
EXTERNAL NAME 'UDF_JAR:UserDefinedFunctions.mybigint(long) returns int';

-- Creating a Java Aggregate Function
CREATE FUNCTION std_dev(
    x FLOAT)
RETURNS FLOAT
CLASS AGGREGATE(79)
LANGUAGE JAVA
NO SQL
PARAMETER STYLE JAVA
EXTERNAL NAME 'UDF_JAR:UserDefinedFunctions.std_dev(
com.teradata.fnc.Phase,
com.teradata.fnc.Context[],
double)
returns java.lang.Double';

-- Using a One-Dimensional ARRAY in a Parameter Definition
CREATE FUNCTION my_array_udf (
    a1 phonenumbers_ary)
RETURNS VARCHAR(100)
NO SQL
PARAMETER STYLE TD_GENERAL
DETERMINISTIC
LANGUAGE C
EXTERNAL NAME 'CS!my_array_udf!my_array_udf.c';
