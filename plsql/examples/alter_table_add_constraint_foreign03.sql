alter table hr.employees
  add constraint emp_fk foreign key (department_id, employee_id) 
  references hr.some_table (department_id, employee_id) 
