create temporary table t(i int);
create table t2(i int) as select(i) from t;
create table t3 as select(i) from t;
create table t1 (v varchar(16777216));

create table t1 (v varchar(16777216) comment 'hello world');
create table t1 (v varchar(16777216) not null comment 'hello world');
create table t1 (v varchar(32) not null unique comment 'hello world');
create table t1 (i integer default 1 comment 'hello world');
create table t1 (i integer not null unique tag (test_tag='t') comment 'hello world');

create table t1 (i integer tag (t='t'));
create table t1 (i integer tag (s.t='s.t'));
create table t1 (i integer tag (d.s.t='d.s.t'));

create table t1 (i integer not null);
create table t1 (i integer not null unique);
create table t1 (i integer unique not null);
create table t1 (i integer primary key not null);
create table t1 (i integer primary key unique not null);
create table t1 (i integer not null unique primary key);

-- Default, collate, not null, and inline constraints can be in any order
create table t1 (i integer not null default 1);
create table t1 (i integer default 1 not null);
create table t1 (i integer not null autoincrement);
create table t1 (i integer autoincrement not null);
create table t1 (v varchar(32) not null collate 'upper');
create table t1 (v varchar(32) collate 'upper' not null);
create table t1 (v varchar(32) unique collate 'upper' not null);
create table t1 (v varchar(32) collate 'upper' unique not null);
create table t1 (v varchar(32) collate 'upper' default 'hey');
create table t1 (v varchar(32) unique collate 'upper' default 'hey');
create table t1 (v varchar(32) default 'hey' unique collate 'upper');
create table t1 (v varchar(32) default 'hey' primary key collate 'upper');

create or replace table t1 (i integer masking policy test_masking_policy);
create or replace table t1 (i integer masking policy test_masking_policy tag (test_tag='t'));
