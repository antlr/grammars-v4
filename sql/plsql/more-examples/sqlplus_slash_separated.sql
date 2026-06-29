-- https://stackoverflow.com/a/10207695/6469038
create table employee_table (
    id        number(12) primary key
)
/  -- ; is optional here
comment on table employee_table is 'employee_table'
/

-----------------------------------------
insert into employee_table values (1)
/ -- ; is optional here
insert into employee_table values (2)
/

-------------------------------------------
create or replace package business_pkg is
    procedure process_employee(
        p_emp_id number
    );
end business_pkg;   -- ; is still required here
/
