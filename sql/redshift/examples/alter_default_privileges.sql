alter default privileges for user report_admin grant select on tables to group report_readers;
alter default privileges grant select on tables to public;
alter default privileges in schema sales grant insert on tables to group sales_admin;
alter default privileges in schema sales revoke insert on tables from group sales_admin;
alter default privileges revoke execute on functions from public;
alter default privileges grant execute on functions to group dev_test;