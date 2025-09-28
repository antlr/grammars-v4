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

CREATE TABLE emp_load (first_name CHAR(15), last_name CHAR(20), year_of_birth CHAR(4))
    ORGANIZATION EXTERNAL ( TYPE ORACLE_LOADER DEFAULT DIRECTORY ext_tab_dir
                            ACCESS PARAMETERS ( FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '(' and ')' LRTRIM )
                            LOCATION ('info.dat'));

CREATE TABLE xtab (recno varchar2(2000))
    ORGANIZATION EXTERNAL ( TYPE ORACLE_LOADER DEFAULT DIRECTORY data_dir
                            ACCESS PARAMETERS ( RECORDS DELIMITED BY NEWLINE
                                                PREPROCESSOR execdir:'zcat'
                                                LOGFILE 'deptxt1.log'
                                                BADFILE 'deptXT.bad'
                                                FIELDS TERMINATED BY ',' MISSING FIELD VALUES ARE NULL (recno char(2000)))
                            LOCATION ('foo.dat.gz')) REJECT LIMIT UNLIMITED;

CREATE TABLE deptxt1 ( deptno number(2), dname varchar2(14), loc varchar2(13))
    ORGANIZATION EXTERNAL ( TYPE ORACLE_LOADER DEFAULT DIRECTORY dpump_dir
                            ACCESS PARAMETERS ( EXTERNAL VARIABLE DATA
                                               LOGFILE 'deptxt1.log'
                                               READSIZE=10000
                                               PREPROCESSOR execdir:'uncompress.sh' )
                            LOCATION ('deptxt1.dmp')) REJECT LIMIT UNLIMITED;

CREATE TABLE "T_XT" ("C0" VARCHAR2(2000))
    ORGANIZATION EXTERNAL ( TYPE ORACLE_LOADER DEFAULT DIRECTORY DMPDIR
                            ACCESS PARAMETERS ( RECORDS XMLTAG ("home address", "work address", "home phone")
                                                READSIZE 1024
                                                SKIP 0
                                                FIELDS NOTRIM MISSING FIELD VALUES ARE NULL)
                            LOCATION ('t.dat')) REJECT LIMIT UNLIMITED;

CREATE TABLE emp_load (first_name CHAR(15), last_name CHAR(20), year_of_birth CHAR(4))
    ORGANIZATION EXTERNAL ( TYPE ORACLE_LOADER DEFAULT DIRECTORY ext_tab_dir
                            ACCESS PARAMETERS ( RECORDS DELIMITED BY '|' FIELDS TERMINATED BY ','
                                              ( first_name CHAR(7), last_name CHAR(8), year_of_birth CHAR(4)))
                            LOCATION ('info.dat'));

CREATE TABLE emp_load (first_name CHAR(15), last_name CHAR(20), year_of_birth CHAR(4))
    ORGANIZATION EXTERNAL ( TYPE ORACLE_LOADER DEFAULT DIRECTORY ext_tab_dir
                            ACCESS PARAMETERS (RECORDS FIXED 20 FIELDS (first_name CHAR(7), last_name CHAR(8), year_of_birth CHAR(4)))
                            LOCATION ('info.dat'));

CREATE TABLE CUSTOMER_TABLE (cust_num VARCHAR2(10), order_num VARCHAR2(20), order_date DATE, item_cnt NUMBER, description VARCHAR2(100), order_total NUMBER(8,2))
    ORGANIZATION EXTERNAL ( TYPE ORACLE_HIVE
                            ACCESS PARAMETERS (
                                com.oracle.bigdata.tableName:  order_db.order_summary
                                com.oracle.bigdata.colMap:     {"col":"ITEM_CNT", "field":"order_line_item_count"}
                                com.oracle.bigdata.overflow:   {"action":"ERROR", "col":"DESCRIPTION"}
                                com.oracle.bigdata.errorOpt:   [{"action":"replace", "value":"INV_NUM" , "col":["CUST_NUM","ORDER_NUM"]} , {"action":"reject", "col":"ORDER_TOTAL"}]
                          ));

CREATE TABLE "MASTERBILL"."ET$03D5D0AD0001" ("USER#", "NAME")
    ORGANIZATION EXTERNAL (
        TYPE ORACLE_DATAPUMP
        DEFAULT DIRECTORY "DBEXPORT"
        ACCESS PARAMETERS (
            DEBUG = (0 , 0)
            DATAPUMP INTERNAL TABLE "SYS"."KU$_USER_MAPPING_VIEW"
            TEMPLATE_TABLE "KU$_USER_MAPPING_VIEW_TBL"
            JOB ( "MASTERBILL","SYS_EXPORT_FULL_01",2)
            WORKERID 1
            PARALLEL 1
            VERSION '19.3.0.0.0'
            ENCRYPTPASSWORDISNULL
            COMPRESSION DISABLED
            ENCRYPTION DISABLED )
        LOCATION ('bogus.dat')
    )
    PARALLEL 1
    REJECT LIMIT UNLIMITED
    AS SELECT X FROM "SYS"."KU$_USER_MAPPING_VIEW" KU$;