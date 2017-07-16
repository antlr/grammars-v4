alter table hr.employees
  add constraint emp_uq unique (employee_id, email) 
