CREATE TABLE deptXTec3
    ORGANIZATION EXTERNAL ( TYPE ORACLE_DATAPUMP DEFAULT DIRECTORY def_dir1
                            ACCESS PARAMETERS (COMPRESSION ENABLED)
                            LOCATION ('dept.dmp'));

CREATE TABLE inv_part_all_xt (PRODUCT_ID NUMBER(6), WAREHOUSE_ID NUMBER(3), QUANTITY_ON_HAND NUMBER(8))
    ORGANIZATION EXTERNAL ( TYPE ORACLE_DATAPUMP DEFAULT DIRECTORY def_dir1
                            ACCESS PARAMETERS (COMPRESSION ENABLED)
                            LOCATION ('inv_p1_xt.dmp','inv_p2_xt.dmp'));

CREATE TABLE emp_load (first_name CHAR(15), last_name CHAR(20), resume CHAR(2000), picture RAW (2000))
    ORGANIZATION EXTERNAL ( TYPE ORACLE_LOADER DEFAULT DIRECTORY ext_tab_dir
                            ACCESS PARAMETERS (FIELDS (first_name VARCHARC(5,12), last_name VARCHARC(2,20), resume VARCHARC(4,10000), picture VARRAWC(4,100000)))
                            LOCATION ('info.dat'));

CREATE TABLE emp_load (first_name CHAR(15), last_name CHAR(20), resume CHAR(2000), picture RAW (2000))
    ORGANIZATION EXTERNAL ( TYPE ORACLE_LOADER DEFAULT DIRECTORY ext_tab_dir
                            ACCESS PARAMETERS ( RECORDS VARIABLE 2 DATA IS BIG ENDIAN CHARACTERSET US7ASCII
                                                FIELDS (first_name VARCHAR(2,12), last_name VARCHAR(2,20), resume VARCHAR(4,10000), picture VARRAW(4,100000)))
                            LOCATION ('info.dat'));

CREATE TABLE emp_load (first_name CHAR(15), last_name CHAR(20), year_of_birth INT, phone CHAR(12), area_code CHAR(3), exchange CHAR(3), extension CHAR(4))
    ORGANIZATION EXTERNAL ( TYPE ORACLE_LOADER DEFAULT DIRECTORY ext_tab_dir
                            ACCESS PARAMETERS ( FIELDS RTRIM (first_name (1:15) CHAR(15), last_name (*:+20), year_of_birth (36:39), phone (40:52), area_code (*-12: +3), exchange (*+1: +3), extension (*+1: +4)))
                            LOCATION ('info.dat'));

CREATE TABLE fruit (
    id NUMERIC(9,0) PRIMARY KEY ANNOTATIONS (Visibility 'Everyone'));

CREATE TABLE fruit (
    id NUMERIC(9,0) PRIMARY KEY,
    data varchar2(50))
    ANNOTATIONS (Visibility 'Everyone');