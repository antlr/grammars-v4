create dynamic table dt1
       target_lag = downstream
       warehouse = wh1
       as select 1 as c;

alter dynamic table dt1 suspend;
alter dynamic table dt1 refresh;
alter dynamic table dt1 set warehouse = wh2;
alter dynamic table dt1 resume;
ALTER DYNAMIC TABLE dt1 SET TARGET_LAG = DOWNSTREAM  WAREHOUSE = wh1 MAX_DATA_EXTENSION_TIME_IN_DAYS = 2;
ALTER DYNAMIC TABLE dt1 UNSET DATA_RETENTION_TIME_IN_DAYS, MAX_DATA_EXTENSION_TIME_IN_DAYS, COMMENT;
alter dynamic table dt1 SWAP WITH DT2;
alter dynamic table dt1 rename to DT2;
ALTER DYNAMIC TABLE DT1 DROP ROW ACCESS POLICY PUBLIC.RLS_STRING;
ALTER DYNAMIC TABLE DT1 ADD ROW ACCESS POLICY PUBLIC.RLS_STRING ON (C1);
ALTER DYNAMIC TABLE DT1 DROP ROW ACCESS POLICY PUBLIC.RLS_STRING , ADD ROW ACCESS POLICY PUBLIC.RLS_STRING ON (C1);

describe dynamic table dt1;

show dynamic tables;
show dynamic tables like 'dt%';
show dynamic tables in account;

drop dynamic table dt1;

create dynamic table dt1 (c1 int WITH MASKING POLICY mpol  TAG ( T = 'mytag'))
       target_lag = downstream
       warehouse = wh1
       as select 1;

create transient dynamic table dt1 (c1)
       warehouse = wh1
       DATA_RETENTION_TIME_IN_DAYS = 1 
       target_lag = downstream
       as select 1;

create dynamic table dt1 (c1 int  WITH MASKING POLICY mpol  TAG ( T = 'mytag') COMMENT 'com')
       target_lag = '1 days'
       warehouse = wh1
       DATA_RETENTION_TIME_IN_DAYS = 1
       MAX_DATA_EXTENSION_TIME_IN_DAYS = 1
       COMMENT = 'Table Com'
       REFRESH_MODE = AUTO
       INITIALIZE = ON_SCHEDULE
       CLUSTER BY (C1)
       WITH ROW ACCESS POLICY RLS ON ( C1 )
       WITH TAG ( T2 = 'ValT2')       
       as select 1;
