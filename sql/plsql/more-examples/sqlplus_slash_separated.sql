-- https://stackoverflow.com/a/10207695/6469038
create table employee_table (
    id        number(12) primary key
)-- ; is optional here
/
comment on table employee_table is 'employee_table'
/

insert into employee_table values (1)
-- ; is optional here
/
insert into employee_table values (2)
/

begin
     null;
end;
/

--this is valid in sqlplus
select 1
/2 from dual
/

select 1
/* multi
line comment */ /
2 from dual
/
