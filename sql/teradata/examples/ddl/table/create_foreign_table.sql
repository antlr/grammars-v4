-- Creating Foreign Table statement
CREATE FOREIGN TABLE some_json
USING ( LOCATION ('/s3/td-b-public.s3.amazonaws.com/JSONDATA/') );

--  Creating Foreign Table with Payload Column with Latin Characters and external security
CREATE MULTISET FOREIGN TABLE mydata,
    EXTERNAL SECURITY MyAuthObj(
    Location VARCHAR(2048) CHARACTER SET UNICODE CASESPECIFIC,
    Payload JSON(16776192) INLINE LENGTH 64000 CHARACTER SET LATIN
    )
USING (
    LOCATION ('/S3/MY-BUCKET.s3.amazonaws.com/data/logs')
    PATHPATTERN ('$Var1/$Var2/$var3/$Var4/$date')
)
NO PRIMARY INDEX
PARTITION BY COLUMN;

-- Creating Foreign Table without payload column
CREATE FOREIGN TABLE sales_parquet
, EXTERNAL SECURITY MyAuthObj
    (
    Location VARCHAR(2048) CHARACTER SET UNICODE CASESPECIFIC
    , site_no BIGINT
    , datetime VARCHAR(16) CHARACTER SET UNICODE CASESPECIFIC
    , quantity DOUBLE PRECISION
    )
USING (
    LOCATION ('/S3/SALES-BUCKET.s3.amazonaws.com/PARQUETDATA')
    STOREDAS ('PARQUET')
) NO PRIMARY INDEX
PARTITION BY COLUMN ;