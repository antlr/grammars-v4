alter table hr.employees
  add foreign key (department_id) 
  references hr.departments (department_id) 
  on delete set null
