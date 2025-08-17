-- Creating a Global Temporary Trace Table
CREATE GLOBAL TEMPORARY TRACE TABLE udf_test (
    proc_ID BYTE(2),
    sequence INTEGER,
    udf_name CHARACTER(15),
    in_quantity INTEGER,
    trace_val1 FLOAT,
    trace_text CHARACTER(30))
ON COMMIT DELETE ROWS;

-- Creating a Trace Table The following example defines a simple trace table
CREATE MULTISET GLOBAL TEMPORARY TRACE TABLE tracetst,
    NO FALLBACK, NO LOG (
    proc_id BYTE(2),
    sequence INTEGER,
    trace_str VARCHAR(100))
ON COMMIT PRESERVE ROWS;
