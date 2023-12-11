create table alert(i int);
create table condition(i int);
create table dt_t(dynamic int, downstream int, target_lag int);
create table wh_id(MAX_CONCURRENCY_LEVEL int, WAREHOUSE_TYPE int);
CREATE TABLE CAST (KEY int,TRY_CAST int);
CREATE TABLE DIRECTION (LENGTH INT,LANGUAGE INT);
CREATE or replace TEMP TABLE ROLES (USERS INT,WAREHOUSES int,INTEGRATIONS int,DATABASES int,SCHEMAS int);
CREATE or replace TEMP TABLE TABLES (VIEWS INT,STAGES int,STREAMS int,TASKS int,ALERTS int);
CREATE or replace TEMP TABLE ACTION (MODE int);