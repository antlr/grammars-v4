create external table t3 (c char(1), v varchar(1) )
    row format delimited
        FIELDS TERMINATED BY ','
        null defined  as 'n'
LOCATION 's3://bucket/loc'
