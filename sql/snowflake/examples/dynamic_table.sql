create dynamic table dt1
       target_lag = downstream
       warehouse = wh1
       as select 1 as c;

alter dynamic table dt1 suspend;
alter dynamic table dt1 refresh;
alter dynamic table dt1 set warehouse = wh2;
alter dynamic table dt1 resume;

describe dynamic table dt1;

show dynamic tables;
show dynamic tables like 'dt%';
show dynamic tables in account;

drop dynamic table dt1;
