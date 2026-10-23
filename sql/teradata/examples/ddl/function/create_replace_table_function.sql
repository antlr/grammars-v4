-- Creating a Function from Input Arguments
CREATE FUNCTION xml_extract( xml_text VARCHAR(64000))
RETURNS TABLE (cust_id INTEGER,
                store INTEGER,
                item INTEGER)
LANGUAGE C
NO SQL
EXTERNAL;

-- Creating and Using an XML Function
CREATE FUNCTION xml_extract( xml_text CLOB AS LOCATOR)
    RETURNS TABLE (cust_id INTEGER,
                    store INTEGER,
                    item INTEGER)
LANGUAGE C
NO SQL
EXTERNAL;

-- Table Function With a UDT Parameter that Returns a Column With a UDT Data Type
CREATE FUNCTION fun_udt_ret(
            p1 INTEGER,
            p2 TABLEINT)
    RETURNS TABLE (c1 INTEGER,
                    c2 INTEGER,
                    c3 VARCHAR(3),
                    udtc4 TABLEINT)
LANGUAGE C
NO SQL
PARAMETER STYLE SQL
EXTERNAL NAME 'CS!fnc_tbf001udt!fnc_tbf001udt.c';

-- Different LOB Specifications for Parameter and RETURNS TABLE Clauses
CREATE FUNCTION lobtf_concat3 (
                    NumRows INTEGER,
                    A BLOB AS LOCATOR,
                    B VARBYTE(64000),
                    C BLOB AS LOCATOR)
RETURNS TABLE (ampnum INTEGER,
                a_out BLOB(10),
                b_out VARBYTE(10),
                c_out BLOB(10),
                myresult BLOB(30))
LANGUAGE C
NO SQL
PARAMETER STYLE SQL
EXTERNAL NAME 'SS!lobtf_concat3!/home/i18n/hsf/tf/c/lobtf_concat3.c';

-- Dynamic Result Row Specification
CREATE FUNCTION extract_store_data (
                text VARCHAR(32000),
                from_store INTEGER)
RETURNS TABLE VARYING COLUMNS (10)
SPECIFIC extract_store_data
LANGUAGE C
NO SQL
PARAMETER STYLE SQL
NOT DETERMINISTIC
EXTERNAL NAME 'SS!extract_store_data!extract_store_data.c';

-- Java Table UDF
CREATE FUNCTION mytableudf (
            p1 INTEGER )
RETURNS TABLE (c1 INTEGER,
                c2 INTEGER)
LANGUAGE JAVA
NO SQL
PARAMETER STYLE JAVA
EXTERNAL NAME 'UDF_JAR:UserDefinedFunctions.mytableudf';

-- Table Operator Function Written in SAS
CREATE FUNCTION sas_transform ()
RETURNS TABLE VARYING USING FUNCTION
LANGUAGE SAS
PARAMETER STYLE SQLTABLE
DETERMINISTIC
--RETURN 'optional SAS code text'  --error in example?
EXTERNAL NAME 'sas_transform_udf:sas_transform_UDF.GetSASCodeText';

-- Ordering Input Parameters to a Table Function
REPLACE FUNCTION add2int (
            a INTEGER,
            b INTEGER)
RETURNS TABLE (
            addend1 INTEGER,
            addend2 INTEGER,
            mysum INTEGER)
SPECIFIC add2int
LANGUAGE C
NO SQL
PARAMETER STYLE SQL
NOT DETERMINISTIC
CALLED ON NULL INPUT
EXTERNAL NAME 'CS!add3int!add2int.c';

-- Using a Table Function to Pass Data Between Two Vantage Systems
CREATE FUNCTION rdg.tdat
    (rowc INTEGER,
    InLineNum INTEGER,
    logonstr VARCHAR(50) CHARACTER SET LATIN,
    sqlRqst VARCHAR(512) CHARACTER SET LATIN)
RETURNS TABLE (
        ampId INTEGER,
        cnt INTEGER,
        OutLineNum INTEGER,
        str1 VARCHAR(256) CHARACTER SET LATIN,
        str2 VARCHAR(256) CHARACTER SET LATIN)
SPECIFIC tdat
LANGUAGE C
NO SQL
PARAMETER STYLE SQL
NOT DETERMINISTIC
CALLED ON NULL INPUT
EXTERNAL NAME 'SS:tdat:/home/rdg/tdat/Tdat.c:SL:cliv2';
