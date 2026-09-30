declare
  l_res integer;
begin
  select 1 into l_res from dual order by $if true $then 1;
end;
