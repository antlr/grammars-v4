--Keyword tester
CREATE TABLE TESTKEYWORD(IFNULL int, NVL int, GET int, LEFT int,RIGHT int, DATE_PART int,TO_DATE int,DATE int,SPLIT int,NULLIF int,EQUAL_NULL int );

create temporary table t(i int);
create table t1 (v varchar(16777216));
create table t2(i int) as select(i) from t;
create table t3 as select(i) from t;

create table t1 (v varchar(16777216) comment 'hello world');
create table t1 (v varchar(16777216) not null comment 'hello world');
create table t1 (v varchar(32) not null unique comment 'hello world');
create table t1 (i integer default 1 comment 'hello world');

create table t1 (i integer tag (t='t'));
create table t1 (i integer tag (s.t='s.t'));
create table t1 (i integer tag (d.s.t='d.s.t'));

-- Default, collate, not null, and inline constraints can be in any order
create table t1 (i integer not null);
create table t1 (i integer not null unique);
create table t1 (i integer unique not null);
create table t1 (i integer primary key not null);
create table t1 (i integer primary key unique not null);
create table t1 (i integer not null unique primary key);
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

create or replace table t1 (i integer masking policy m);
create or replace table t1 (i integer masking policy m tag (t='t'));

create table t (i integer default 1 not null unique masking policy m tag (t='t') comment 'hello world');
create table t (v varchar unique not null collate 'upper' masking policy m tag (t='t') comment 'hello world');
create table public.public.public (public int);
CREATE TABLE T ( Source string NOT NULL, Query_Id string NULL,	State string NOT NULL,Procedure_Name string);
create table if not exists t1 (v varchar(16777216));
create table t1 if not exists (v varchar(16777216));
create table if not exists t2(i int) as select(v) from t1;
create table t2 if not exists (i int) as select(v) from t1;
CREATE OR REPLACE TABLE TESTSEED (IDENT int DEFAULT SEQID.NEXTVAL,mycol string);
CREATE OR REPLACE TABLE TESTSEED (IDENT int DEFAULT SCHEM.SEQID.NEXTVAL,mycol string);
CREATE OR REPLACE TABLE TESTSEED2 (ident int IDENTITY START 2);
CREATE OR REPLACE TABLE TESTSEED2 (ident int IDENTITY START WITH = 2);
CREATE OR REPLACE TABLE TESTSEED2 (ident int IDENTITY START = 2 INCREMENT BY 1);
CREATE OR REPLACE TABLE TESTSEED2 (ident int IDENTITY INCREMENT 2);
create table t1 (v datetime(9));
CREATE TABLE T1 (TIMESTAMP DATETIME,VALUE STRING,NAME STRING);

-- outline constraint
CREATE OR REPLACE TABLE T1 (C1 STRING,UNIQUE(C1));
CREATE OR REPLACE TABLE T2 (C2 STRING,PRIMARY KEY(C2));
CREATE OR REPLACE TABLE T3 (C3 STRING,FOREIGN KEY(C3) REFERENCES T2(C2));
CREATE OR REPLACE TABLE T1 (C1 STRING,CONSTRAINT ANAME UNIQUE(C1));
CREATE OR REPLACE TABLE T2 (C2 STRING,CONSTRAINT BNAME PRIMARY KEY(C2));
CREATE OR REPLACE TABLE T3 (C3 STRING,CONSTRAINT CNAME FOREIGN KEY(C3) REFERENCES T2(C2));
-- constraint properties
CREATE OR REPLACE TABLE T1 (C1 STRING,CONSTRAINT ANAME UNIQUE(C1) RELY ENFORCED VALIDATE );
CREATE OR REPLACE TABLE T1 (C1 STRING,CONSTRAINT ANAME UNIQUE(C1) INITIALLY IMMEDIATE NOT DEFERRABLE );
CREATE OR REPLACE TABLE T3 (C3 STRING,CONSTRAINT CNAME FOREIGN KEY(C3) REFERENCES T2(C2) MATCH FULL ON DELETE CASCADE ON UPDATE CASCADE);
CREATE OR REPLACE TABLE T3 (C3 STRING,CONSTRAINT CNAME FOREIGN KEY(C3) REFERENCES T2(C2) MATCH PARTIAL ON UPDATE SET NULL ON DELETE RESTRICT );
CREATE OR REPLACE TABLE T3 (C3 STRING,CONSTRAINT CNAME FOREIGN KEY(C3) REFERENCES T2(C2) ON UPDATE NO ACTION  );
CREATE OR REPLACE TABLE T3 (C3 STRING,CONSTRAINT CNAME FOREIGN KEY(C3) REFERENCES T2(C2) DEFERRABLE );
CREATE OR REPLACE TABLE T1 (C1 STRING UNIQUE INITIALLY IMMEDIATE NOT DEFERRABLE );
CREATE OR REPLACE TABLE T3 (C3 STRING FOREIGN KEY REFERENCES T2(C2) DEFERRABLE );
CREATE OR REPLACE TABLE T3 (C3 STRING CONSTRAINT INCNAME FOREIGN KEY REFERENCES T2(C2) ON UPDATE NO ACTION  );
CREATE OR REPLACE TABLE T3 (C3 STRING FOREIGN KEY REFERENCES T2 MATCH PARTIAL ON UPDATE SET NULL ON DELETE RESTRICT );
create table TestK (NVL2 string, FIRST_VALUE string, RESTRICT int, NVL int, RESPECT int);

Create table T1(C1 string) WITH TAG ( TAG_NAME='T1');
create table t
as
with q as (
    select 1 as c
)
select c from q;

create table tpk (i int primary key);
alter table tpk drop primary key;

create table tc1 comment = '' (i int);
create table tc2 (i int) comment = '';
create table tc3 (c char(4), c2 character(2));

create or replace table tz1(i TIMESTAMPLTZ);
create or replace table tz2(i TIMESTAMPNTZ);
create or replace table tz3(i TIMESTAMPTZ);
CREATE TABLE TESTSEED2 (ident int IDENTITY INCREMENT 2 ORDER);
CREATE TABLE TESTSEED2 (ident int IDENTITY START = 2 INCREMENT BY 1 NOORDER);
CREATE TABLE DIRECTION (LENGTH INT,LANGUAGE INT);