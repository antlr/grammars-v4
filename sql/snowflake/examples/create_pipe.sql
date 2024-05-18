-- internal stages
-- named stage without schema
create or replace pipe P1 auto_ingest=false as COPY INTO TABLE1 FROM @STAGE1;
--named stage with schema
create or replace pipe P2 auto_ingest=false as COPY INTO SCH1.TABLE1 FROM @SCH1.STAGE1;
-- named stage with schema and path
create or replace pipe P3 auto_ingest=false as COPY INTO TABLE1 FROM @SCH1.STAGE1/dir1/dir2/;
create or replace pipe P4 auto_ingest=false as COPY INTO TABLE1 FROM @SCH1.STAGE1/;

-- snowflake docs examples
create pipe mypipe as copy into mytable from @mystage;

create pipe mypipe2 as copy into mytable(C1, C2) from (select $5, $4 from @mystage);

create pipe mypipe3 as copy into mytable(C1, C2) from (select $5, $4 from @mysch.mystage/x/y/z);

create pipe mypipe_s3
  auto_ingest = true
  aws_sns_topic = 'arn:aws:sns:us-west-2:001234567890:s3_mybucket'
  as
  copy into snowpipe_db.public.mytable
  from @snowpipe_db.public.mystage
  file_format = (type = 'JSON');

create pipe mypipe_gcs
  auto_ingest = true
  integration = 'MYINT'
  as
  copy into snowpipe_db.public.mytable
  from @snowpipe_db.public.mystage
  file_format = (type = 'JSON');  

create pipe mypipe_azure
  auto_ingest = true
  integration = 'MYINT'
  as
  copy into snowpipe_db.public.mytable
  from @snowpipe_db.public.mystage
  file_format = (type = 'JSON');  