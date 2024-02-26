create database d1;
use d1;

create event table et1;

alter table et set comment = 't';

show event tables like 'et1';

desc event table et1;

drop table et1;
