alter table hr.employees
  add constraint emp_fk foreign key (department_id) 
  references hr.departments (department_id) 
  on delete cascade;

alter table hr.employees
  add foreign key (department_id) 
  references hr.departments (department_id) 
  on delete set null;

alter table hr.employees
  add constraint emp_fk foreign key (department_id, employee_id) 
  references hr.some_table (department_id, employee_id);

alter table employees
  add primary key (employee_id);

alter table hr.employees
  add constraint emp_complex_pk primary key (employee_id, department_id);

alter table hr.employees
  add constraint emp_uq unique (employee_id, email);

alter table employees
  add unique (employee_id);

ALTER TABLE suppliers
ADD CONSTRAINT check_supplier_name
  CHECK (supplier_name IN ('IBM', 'Microsoft', 'NVIDIA'));
  
ALTER TABLE suppliers
  DROP CONSTRAINT check_supplier_id;

ALTER TABLE suppliers
  ENABLE CONSTRAINT check_supplier_id;

ALTER TABLE suppliers
  DISABLE CONSTRAINT check_supplier_id;

ALTER TABLE customers
   PARALLEL;

ALTER TABLE employees
    DEALLOCATE UNUSED;

ALTER TABLE countries_demo INITRANS 4;

ALTER TABLE employees 
   PCTFREE 30
   PCTUSED 60; 

ALTER TABLE employees
  ALLOCATE EXTENT (SIZE 5K INSTANCE 4);

ALTER TABLE customers
   ADD (online_acct_pw VARCHAR2(8) ENCRYPT 'NOMAC' NO SALT );

ALTER TABLE employees ADD (resume CLOB)
  LOB (resume) STORE AS resume_seg (TABLESPACE example);

ALTER TABLE employees ADD (resume CLOB)
LOB (resume) STORE AS SECUREFILE resume_seg (TABLESPACE auto_seg_ts);

ALTER TABLE employees ADD (skills number)
    NESTED TABLE skills STORE AS nested_skill_table;

