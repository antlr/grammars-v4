/* DROP */
drop table t;
drop table s.t;
drop table t cascade ;
drop table if exists t;
drop table if exists t cascade ;

drop view v;
drop view s.v;
drop view v cascade ;
drop view if exists v;

drop function f;
drop function if exists f;

drop sequence s.s;
drop sequence s;
drop sequence if exists s;

drop index i on t;
drop index if exists i on t;
drop index if exists i on s.t;

drop schema s;
drop schema if exists s;

/* CREATE */
create table t (i integer, c char(1) );
create table t (i integer, c char(1) ) STORE_NULLS = true, DISABLE_WAL = false;
create table s.t (i integer, c char(1) );
create table s.t (i integer, t time, c char(1) constraint pk primary key (i asc, t desc) );
create table if not exists s.t (i integer primary key , c char(1) );
create table if not exists s.t (i integer primary key , c char(1) ) split on (1);

create function f (integer ) returns integer as 'org.apache.Dummy';
create temporary function f (integer, bigint) returns integer as 'org.apache.Dummy';
create function f (integer ) returns integer as 'org.apache.Dummy' using jar 'dummy.jar';

create view v;
create view v (i integer primary key, bi bigint) as select * from t;
create view v (i integer primary key, bi bigint) as select * from t where c = 1;
create view if not exists s.v;

create schema s;
create schema if not exists s;

--create index i on t(c asc);
--create index if not exists s.i on s.t(c asc, d desc);

create sequence s;
create sequence s.s;
create sequence if not exists s;
create sequence if not exists s start with 1 increment by 2 cache 2;
create sequence if not exists s start with 1 increment by 2 minvalue 1 maxvalue 10 cycle cache 2;

/* CURSOR */
declare cursor c for select * from t;
open cursor c;
fetch next 1 rows from c;
fetch next from c;
close c;
