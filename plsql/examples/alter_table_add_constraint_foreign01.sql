alter table hr.employees
  add constraint emp_fk foreign key (department_id) 
  references hr.departments (department_id) 
  on delete cascade
