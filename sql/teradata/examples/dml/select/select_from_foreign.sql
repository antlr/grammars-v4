--Teradata to Presto SELECT
SELECT CAST(Price AS DECIMAL (8,2))
, mileage
, CAST(make AS VARCHAR(20))
, CAST(model AS VARCHAR(20))
FROM cardata@presto1 WHERE make='Lada';

-- Teradata to Presto SELECT from Foreign Table
SELECT * FROM FOREIGN TABLE (SELECT make, model FROM default.cardata
                            WHERE make = 'Volga')@QG_presto1 AS dt;

-- Teradata to Presto SELECT from Foreign Table with an EXPORT Clause
SELECT * FROM FOREIGN TABLE (SELECT datatypes1.bigint1, temp.double1
                             FROM datatypes1, temp
                             WHERE datatypes1.bigint1 > 1)@test
             EXPORT((select * from ut2.datatypes2) as temp) AS ft;

-- Teradata to Presto SELECT from Foreign Table with an EXPORT Clause (Multiple Input Streams)
SELECT * FROM FOREIGN TABLE(SELECT types_numeric.bigint1,temp1.integer1 , temp2.byteint2
                          FROM types_numeric, temp1,temp2
                          WHERE types_numeric.bigint1 > 1
                            AND temp2.bigint2 = types_numeric.bigint1)@td1
               EXPORT( (SELECT 127 as byteint1
                             , 32767 as smallint1
                             , 2147483647 as integer1
                             , 9223372036854775807 as bigint1) as temp1
                        , localdb.table1 as temp2) AS ft;

-- SELECT with a RETURNS Clause
SELECT *
  FROM datatypes1@fs1 RETURNS (varchar1 VARCHAR(5) CHARACTER SET LATIN)
 WHERE bigint1 >10000 AND double1 < 10000;

-- SELECT Foreign Function
SELECT clicktime, custname(char(10)), productname, pagetype, productprice, sessionid
FROM SESSIONIZE@remoteTD (
    ON ( sess_data )
    PARTITION BY partition_id
    ORDER BY clicktime
    USING
    TIMECOLUMN('clicktime')
    TIMEOUT(60)
    RAPIDFIRE(0.2)
    ) AS S, cust_data where S.userid = custid ORDER BY S.partition_id, S.clicktime;
