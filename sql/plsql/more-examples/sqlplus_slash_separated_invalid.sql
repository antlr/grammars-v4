-- https://stackoverflow.com/a/10207695/6469038
create or replace package business_pkg is
    procedure process_employee(
        p_emp_id number
    );
end business_pkg -- ; is still required here
/
