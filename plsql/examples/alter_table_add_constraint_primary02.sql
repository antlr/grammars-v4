alter table hr.employees
  add constraint emp_complex_pk primary key (employee_id, department_id) 
