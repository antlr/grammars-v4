-- Time travel https://docs.snowflake.com/en/sql-reference/constructs/at-before.html
-- Examples from doc
create table my_table (id int, flag varchar(50));  -- Create table to silence linter
select * from my_table at(timestamp => 'Fri, 01 May 2015 16:20:00 -0700'::timestamp);
select * from my_table at(timestamp => to_timestamp(1432669154242, 3));
select * from my_table at(offset => -60*5) as t where t.flag = 'valid';
select * from my_table before(statement => '8e5d0ca9-005e-44e6-b858-a8f5b37c5726');
select oldt.* ,newt.*
  from my_table before(statement => '8e5d0ca9-005e-44e6-b858-a8f5b37c5726') as oldt
    full outer join my_table at(statement => '8e5d0ca9-005e-44e6-b858-a8f5b37c5726') as newt
    on oldt.id = newt.id
where oldt.id is null or newt.id is null;
