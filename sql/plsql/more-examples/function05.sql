select count(*)
  from employees
  where lnnvl(commission_pct >= .2);

select count(distinct employe_id)
  from employees;
