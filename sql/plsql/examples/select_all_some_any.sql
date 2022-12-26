
select * from "CW_ROLE" where role_id = some(1,5);

select * from "T_ROLE" where role_id = any(1,5);

select * from "T_ROLE" where role_id = all(1,5);

