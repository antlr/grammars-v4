#begin
-- Create Table
create table new_t  (like t1);
create table log_table(row varchar(512));
create table log_table(row character(512));
create table ships(name varchar(255), class_id int, id int);
create table ships_guns(guns_id int, ship_id int);
create table guns(id int, power decimal(7,2), callibr decimal(10,3));
create table ship_class(id int, class_name varchar(100), tonange decimal(10,2), max_length decimal(10,2), start_build year, end_build year(4), max_guns_size int);
create table `some table $$`(id int auto_increment key, class varchar(10), data binary) engine=MYISAM;
create table quengine(id int auto_increment key, class varchar(10), data binary) engine='InnoDB';
create table quengine(id int auto_increment key, class varchar(10), data binary) engine="Memory";
create table quengine(id int auto_increment key, class varchar(10), data binary) engine=`CSV`;
create table quengine(id int auto_increment key, class varchar(10), data binary COMMENT 'CSV') engine=MyISAM;
create table quengine(id int auto_increment key, class varchar(10), data binary) engine=Aria;
create table `parent_table`(id int primary key, column1 varchar(30), index parent_table_i1(column1(20)), check(char_length(column1)>10)) engine InnoDB;
create table child_table(id int unsigned auto_increment primary key, id_parent int references parent_table(id) match full on update cascade on delete set null) engine=InnoDB;
create table `another some table $$` like `some table $$`;
create table `actor` (`last_update` timestamp default CURRENT_TIMESTAMP, `birthday` datetime default CURRENT_TIMESTAMP ON UPDATE LOCALTIMESTAMP);
create table boolean_table(c1 bool, c2 boolean default true);
create table default_table(c1 int default 42, c2 int default -42, c3 varchar(256) DEFAULT _utf8mb3'xxx');
create table ts_table(
  ts1 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  ts2 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE LOCALTIME,
  ts3 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE LOCALTIMESTAMP,
  ts4 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP(),
  ts5 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE LOCALTIME(),
  ts6 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE LOCALTIMESTAMP(),
  ts7 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE NOW(),
  ts8 TIMESTAMP(6) NOT NULL,
  ts9 TIMESTAMP(6) NOT NULL DEFAULT NOW(6) ON UPDATE NOW(6),
  ts10 TIMESTAMP DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP,
  ts11 TIMESTAMP NOT NULL DEFAULT '2038-01-01 00:00:00' ON UPDATE CURRENT_TIMESTAMP
);
create table dt_table(
  dt1 DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  dt2 DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE LOCALTIME,
  dt3 DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE LOCALTIMESTAMP,
  dt4 DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP(),
  dt5 DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE LOCALTIME(),
  dt6 DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE LOCALTIMESTAMP(),
  dt7 DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE NOW(),
  dt10 DATETIME DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP,
  dt11 DATETIME DEFAULT '2038-01-01 00:00:00' ON UPDATE CURRENT_TIMESTAMP
);
create table with_check (c1 integer not null,c2 varchar(22),constraint c1 check (c2 in ('a', 'b', 'c')));
CREATE TABLE genvalue1 (id binary(16) NOT NULL, val char(32) GENERATED ALWAYS AS (hex(id)) STORED, PRIMARY KEY (id));
CREATE TABLE genvalue2 (id binary(16) NOT NULL, val char(32) AS (hex(id)) STORED, PRIMARY KEY (id));
CREATE TABLE genvalue3 (id binary(16) NOT NULL, val char(32) GENERATED ALWAYS AS (hex(id)) VIRTUAL, PRIMARY KEY (id));
CREATE TABLE cast_charset (col BINARY(16) GENERATED ALWAYS AS (CAST('xx' as CHAR(16) CHARACTER SET BINARY)) VIRTUAL);
CREATE TABLE cast_charset (col BINARY(16) GENERATED ALWAYS AS (CAST('xx' as CHAR(16) CHARSET BINARY)) VIRTUAL);
CREATE TABLE check_table_kw (id int primary key, upgrade varchar(256), quick varchar(256), fast varchar(256), medium varchar(256), extended varchar(256), changed varchar(256));
CREATE TABLE sercol1 (id SERIAL, val INT);
CREATE TABLE sercol2 (id SERIAL PRIMARY KEY, val INT);
CREATE TABLE sercol3 (id SERIAL NULL, val INT);
CREATE TABLE sercol4 (id SERIAL NOT NULL, val INT);
CREATE TABLE serval1 (id SMALLINT SERIAL DEFAULT VALUE, val INT);
CREATE TABLE serval2 (id SMALLINT SERIAL DEFAULT VALUE PRIMARY KEY, val INT);
CREATE TABLE serval3 (id SMALLINT(3) NULL SERIAL DEFAULT VALUE, val INT);
CREATE TABLE serval4 (id SMALLINT(5) UNSIGNED SERIAL DEFAULT VALUE NOT NULL, val INT);
CREATE TABLE serial (serial INT);
CREATE TABLE float_table (f1 FLOAT, f2 FLOAT(10), f3 FLOAT(7,4));
CREATE TABLE USER (INTERNAL BOOLEAN DEFAULT FALSE);
create table table_with_character_set_eq (id int, data varchar(50)) character set = default;
create table table_with_character_set (id int, data varchar(50)) character set default;
create table table_with_visible_index (id int, data varchar(50), UNIQUE INDEX `data_UNIQUE` (`data` ASC) INVISIBLE VISIBLE);
create table table_with_index (id int, data varchar(50), UNIQUE INDEX `data_UNIQUE` (`data` ASC));
create table blob_test(id int, col1 blob(45));
create table žluťoučký (kůň int);
create table column_names_as_aggr_funcs(min varchar(100), max varchar(100), sum varchar(100), count varchar(100));
CREATE TABLE char_table (c1 CHAR VARYING(10), c2 CHARACTER VARYING(10), c3 NCHAR VARYING(10));
create table rack_shelf_bin ( id int unsigned not null auto_increment unique primary key, bin_volume decimal(20, 4) default (bin_len * bin_width * bin_height));
CREATE TABLE `tblSRCHjob_desc` (`description_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT, `description` mediumtext NOT NULL, PRIMARY KEY (`description_id`)) ENGINE=TokuDB AUTO_INCREMENT=4095997820 DEFAULT CHARSET=utf8mb4 ROW_FORMAT=TOKUDB_QUICKLZ;
create table invisible_column_test(id int, col1 int INVISIBLE);
create table visible_column_test(id int, col1 int VISIBLE);
CREATE TABLE foo (c1 decimal(19), c2 decimal(19.5), c3 decimal(0.0), c4 decimal(0.2), c5 decimal(19,2));
CREATE TABLE table_items (id INT, purchased DATE)
    PARTITION BY RANGE( YEAR(purchased) )
        SUBPARTITION BY HASH( TO_DAYS(purchased) )
        SUBPARTITIONS 2 (
        PARTITION p0 VALUES LESS THAN (1990),
        PARTITION p1 VALUES LESS THAN (2000),
        PARTITION p2 VALUES LESS THAN MAXVALUE
    );

CREATE TABLE table_items_with_subpartitions (id INT, purchased DATE)
    PARTITION BY RANGE( YEAR(purchased) )
        SUBPARTITION BY HASH( TO_DAYS(purchased) ) (
        PARTITION p0 VALUES LESS THAN (1990) (
            SUBPARTITION s0,
            SUBPARTITION s1
        ),
        PARTITION p1 VALUES LESS THAN (2000) (
            SUBPARTITION s2,
            SUBPARTITION s3
        ),
        PARTITION p2 VALUES LESS THAN MAXVALUE (
            SUBPARTITION s4,
            SUBPARTITION s5
        )
    );

CREATE TABLE positions_rollover (
    id bigint(20) NOT NULL AUTO_INCREMENT,
    time datetime NOT NULL,
    partition_index int(10) unsigned NOT NULL DEFAULT 0,
    PRIMARY KEY (id,partition_index),
    KEY time (time)
) ENGINE=InnoDB DEFAULT CHARSET=utf8
PARTITION BY LIST (partition_index) (
    PARTITION positions_rollover_partition VALUES IN (0) ENGINE = InnoDB,
    PARTITION default_positions_rollover_partition DEFAULT ENGINE = InnoDB
);

CREATE TABLE `tab_with_json_value` (
   `col0` JSON NOT NULL,
   `col1` VARCHAR(36) COLLATE utf8mb4_bin GENERATED ALWAYS AS (
      JSON_VALUE(`col0`, _utf8mb4'$._field1' RETURNING CHAR(36) CHARACTER SET latin1)
   ) STORED NOT NULL,
   `col2` VARCHAR(36) COLLATE utf8mb4_bin GENERATED ALWAYS AS (
      JSON_VALUE(`col0`, _utf8mb4'$._field1' ERROR ON EMPTY)
   ) STORED NOT NULL,
   `col3` VARCHAR(36) COLLATE utf8mb4_bin GENERATED ALWAYS AS (
      JSON_VALUE(`col0`, _utf8mb4'$._field1' DEFAULT 'xx' ON ERROR)
   ) STORED NOT NULL,
   `col4` JSON NOT NULL,
   PRIMARY KEY (`col1`)
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4 COLLATE = utf8mb4_bin ROW_FORMAT = COMPRESSED;

CREATE TABLE CustomerTable (
    CustomerID varchar(5),
    CompanyName varchar(40),
    ContactName varchar(30),
    Address varchar(60),
    Phone varchar(24)
 ) ENGINE = CONNECT TABLE_TYPE = ODBC;

CREATE TABLE tbl (
    col1 LONGTEXT,
    data JSON,
    INDEX idx1 ((SUBSTRING(col1, 1, 10))),
    INDEX idx2 ((CAST(JSON_EXTRACT(data, _utf8mb4'$') AS UNSIGNED ARRAY))),
    INDEX ((CAST(data->>'$.name' AS CHAR(30))))
);

CREATE TABLE `orders_json_2` (
  `id` int NOT NULL AUTO_INCREMENT,
  `reward` json DEFAULT NULL,
  `additional_info` json DEFAULT NULL,
  `created_at` timestamp NULL DEFAULT NULL,
  `updated_at` timestamp NULL DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `idx_order_codes` ((cast(json_extract(`additional_info`,_utf8mb4'$.order_codes') as char(17) array)))
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

CREATE TABLE keywords (
    eur VARCHAR(100),
    iso VARCHAR(10) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
    usa VARCHAR(100),
    jis VARCHAR(100),
    internal INT,
    instant BIT
);

create table if not exists tbl_signed_unsigned(
  `id` bigint(20) ZEROFILL signed UNSIGNED signed ZEROFILL unsigned ZEROFILL NOT NULL AUTO_INCREMENT COMMENT 'ID',
  c1 int signed unsigned,
  c2 decimal(10, 2) SIGNED UNSIGNED ZEROFILL,
  c3 float SIGNED ZEROFILL,
  c4 double precision(18, 4) UNSIGNED SIGNED ZEROFILL,
  PRIMARY KEY (`id`)
);

CREATE TABLE `daily_intelligences`(
`id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '',
`partner_code` varchar(32) DEFAULT NULL COMMENT '',
`text` LONGTEXT DEFAULT NULL COMMENT '',
`monitor_time` TIMESTAMP DEFAULT NULL COMMENT '',
`gmt_modify` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '',
`gmt_create` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '',
PRIMARY KEY (`id`)
) ENGINE=innodb DEFAULT CHAR SET=utf8 COMMENT '';

-- Mariadb default value for function: unix_timestamp()
CREATE TABLE `table_default_fn`(`quote_id` varchar(32) NOT NULL,`created_at` bigint(20) NOT NULL DEFAULT unix_timestamp());
#end
#begin
-- Rename table
-- http://dev.mysql.com/doc/refman/5.6/en/rename-table.html
RENAME TABLE old_table TO tmp_table, new_table TO old_table, tmp_table TO new_table;
RENAME TABLE table_b TO table_a;
RENAME TABLE current_db.tbl_name TO other_db.tbl_name;
#end
#begin
-- Truncate table
truncate table t1;
truncate parent_table;
truncate `#`;
truncate `#!^%$`;
#end
#begin
-- Create database
create database somedb;
create schema if not exists myschema;
create schema `select` default character set = utf8;
create database if not exists `current_date` character set cp1251;
create database super default character set utf8 collate = utf8_bin character set utf8 collate utf8_bin;
create database super_cs default charset utf8 collate = utf8_bin character set utf8 collate utf8_bin;
create database db_with_character_set_eq character set = default;
create database db_with_character_set character set default;
create database `ymsun_test1` charset gb18030 collate gb18030_bin;
create database `test` charset binary collate binary;
#end
#begin
-- Create event 1
-- delimiter //
create definer = current_user event if not exists someevent on schedule at current_timestamp + interval 30 minute
on completion preserve do begin insert into test.t1 values (33), (111);select * from test.t1; end; -- //
#end
#begin
-- Create event 2
create definer = 'ivan'@'%' event testevent1 on schedule every 1 hour ends '2016-11-05 23:59:00'
do begin select * from test.t2; end; -- //
#end
#begin
-- Create event 3
create definer = current_user() event testevent2 on schedule at '2016-11-03 23:59:00'
do begin update test.t2 set 1c = 1c + 1; end; -- //
-- delimiter ;
#end
#begin
-- Create index
create index index1 on t1(col1) comment 'test index' comment 'some test' using btree;
create unique index index2 using btree on t2(1c desc, `_` asc);
create index index3 using hash on antlr_tokens(token(30) asc);
create index index4 on antlr_tokens(token(30) asc) lock default;
create index index5 on antlr_tokens(token(30) asc) algorithm default;
create index index6 on antlr_tokens(token(30) asc) algorithm default lock default;
create index index7 on antlr_tokens(token(30) asc) lock default algorithm default;
-- Create mariadb index
CREATE INDEX IF NOT EXISTS DX_DT_LAST_UPDATE ON patient(DT_LAST_UPDATE) WAIT 100 KEY_BLOCK_SIZE=1024M CLUSTERING =YES USING RTREE NOT IGNORED ALGORITHM = NOCOPY LOCK EXCLUSIVE;
#end
#begin
-- Create logfile group
-- http://dev.mysql.com/doc/refman/5.6/en/create-logfile-group.html
CREATE LOGFILE GROUP lg1 ADD UNDOFILE 'undo.dat' INITIAL_SIZE = 10M ENGINE = InnoDB;
-- CREATE LOGFILE GROUP lg1 ADD UNDOFILE 'undo.dat' INITIAL_SIZE = 10M;
CREATE LOGFILE GROUP lg1 ADD UNDOFILE 'undo.dat' INITIAL_SIZE = 10000000 ENGINE = NDB;
#end
#begin
-- Create server
-- http://dev.mysql.com/doc/refman/5.6/en/create-server.html
CREATE SERVER s
FOREIGN DATA WRAPPER mysql
OPTIONS (USER 'Remote', HOST '192.168.1.106', DATABASE 'test');
#end
#begin
-- Create tablespace
create tablespace tblsp1 add datafile 'tblsp_work1' use logfile group lg_1 initial_size = 4G engine MYISAM;
create tablespace tblsp2 add datafile 'tblsp_proj1' use logfile group lg_6 autoextend_size = 4294 max_size = 2294967296 engine NDB;
#end
#begin
-- Create trigger 1
-- delimiter //
create trigger trg_my1 before delete on test.t1 for each row begin insert into log_table values ("delete row from test.t1"); insert into t4 values (old.col1, old.col1 + 5, old.col1 + 7); end; -- //-- delimiter ;
#end
#begin
-- Create trigger 2
create definer = current_user() trigger trg_my2 after insert on test.t2 for each row insert into log_table values (concat("inserted into table test.t2 values: (1c, _) = (", cast(NEW.col1 as char(100)), ", ", convert(new.`_`, char(100)), ")"));
#end
#begin
-- Create trigger 3
-- delimiter //
CREATE TRIGGER mask_private_data BEFORE INSERT ON users FOR EACH ROW BEGIN SET NEW.phone = CONCAT('555', NEW.id); END; -- //-- delimiter ;
#end
#begin
-- Create trigger 4
-- CAST to JSON
CREATE DEFINER=`ctlplane`@`%` TRIGGER `write_key_add` AFTER INSERT ON `sources` FOR EACH ROW
BEGIN
DECLARE i, n INT DEFAULT 0;
SET n = JSON_LENGTH(CAST(CONVERT(NEW.write_keys USING utf8mb4) AS JSON));
SET campaign_id = NEW.write_keys->>'$.campaign_id';
WHILE i < n DO
INSERT INTO source_id_write_key_mapping (source_id, write_key)
VALUES (NEW.id, JSON_UNQUOTE(JSON_EXTRACT(CAST(CONVERT(NEW.write_keys USING utf8mb4) AS JSON), CONCAT('$[', i, ']'))))
ON DUPLICATE KEY UPDATE
       source_id  = NEW.ID,
       write_key  = JSON_UNQUOTE(JSON_EXTRACT(CAST(CONVERT(NEW.write_keys USING utf8mb4) AS JSON), CONCAT('$[', i, ']')));
SET i = i + 1;
END WHILE;
END
#end
#begin
-- Create trigger 5
CREATE TRIGGER `rtl_trigger_before_update`
BEFORE UPDATE
ON all_student_educator FOR EACH ROW
BEGIN
    IF NEW.student_words_read_total is not null AND NEW.student_words_read_total >= 3 AND NEW.badge_3_words_read_flag = 0 THEN
        SET
        NEW.badge_flag = 1,
        NEW.badge_student_total = NEW.badge_student_total + 1,
        NEW.badge_datetime = now();
        INSERT IGNORE INTO user_platform_badge (platform_badge_id, user_id) VALUES (3, NEW.student_id);
    END IF;
END
#end
#begin
-- Create trigger 6
-- delimiter //
create or replace trigger trg_my1 before delete on test.t1 for each row begin insert into log_table values ("delete row from test.t1"); insert into t4 values (old.col1, old.col1 + 5, old.col1 + 7); end; -- //-- delimiter ;
#end
#begin
-- Create view
create or replace view my_view1 as select 1 union select 2 limit 0,5;
create algorithm = merge view my_view2(col1, col2) as select * from t2 with check option;
create or replace definer = 'ivan'@'%' view my_view3 as select count(*) from t3;
create or replace definer = current_user sql security invoker view my_view4(c1, 1c, _, c1_2) 
	as select * from  (t1 as tt1, t2 as tt2) inner join t1 on t1.col1 = tt1.col1;

#end
#begin
-- Create function
-- delimiter //
CREATE FUNCTION `fivenumbers`() RETURNS varchar(5) CHARSET utf8 COLLATE utf8_unicode_ci DETERMINISTIC
BEGIN
	RETURN '12345';
END; -- //-- delimiter ;
#end
#begin
-- Create function
CREATE FUNCTION `uuidToBinary`(_uuid BINARY(36)) RETURNS binary(16)
    DETERMINISTIC
    SQL SECURITY INVOKER
RETURN
  UNHEX(CONCAT(
            SUBSTR(_uuid, 15, 4),
            SUBSTR(_uuid, 10, 4),
            SUBSTR(_uuid,  1, 8),
            SUBSTR(_uuid, 20, 4),
            SUBSTR(_uuid, 25) ))
#end
#begin
-- From MariaDB 10.4.3, the JSON_VALID function is automatically used as a CHECK constraint for the JSON data type alias in order to ensure that a valid json document is inserted.
-- src: https://mariadb.com/kb/en/json_valid/
CREATE TABLE `global_priv` (
    `Host` CHAR(60) COLLATE utf8_bin NOT NULL DEFAULT '',
    `User` CHAR(80) COLLATE utf8_bin NOT NULL DEFAULT '',
    `Privilege` LONGTEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL DEFAULT '{}' CHECK (json_valid(`Privilege`)),
    PRIMARY KEY (`Host`,`User`)
) ENGINE=Aria DEFAULT CHARSET=utf8 COLLATE=utf8_bin PAGE_CHECKSUM=1 COMMENT='Users and global privileges';
#end
#begin
-- https://dev.mysql.com/doc/refman/8.0/en/json-validation-functions.html#json-validation-functions-constraints
CREATE TABLE geo (
    coordinate JSON,
    CHECK(
        JSON_SCHEMA_VALID(
           '{
               "type":"object",
               "properties":{
                 "latitude":{"type":"number", "minimum":-90, "maximum":90},
                 "longitude":{"type":"number", "minimum":-180, "maximum":180}
               },
               "required": ["latitude", "longitude"]
           }',
           coordinate
        )
    )
);
#end
#begin
CREATE TABLE `tab1` (
  f4 FLOAT4,
  f8 FLOAT8,
  i1 INT1,
  i2 INT2,
  i3 INT3,
  i4 INT4,
  i8 INT8,
  lvb LONG VARBINARY,
  lvc LONG VARCHAR,
  lvcfull LONG BINARY CHARSET utf8 COLLATE utf8_bin,
  l LONG,
  mi MIDDLEINT
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
#end
-- Comments
-- SELECT V_PAYABLE_AMT, DIS_ADJUST_TOTAL_PAYABLE;
--	SELECT V_PAYABLE_AMT, DIS_ADJUST_TOTAL_PAYABLE;
#begin
-- Create procedure
-- The default value for local variables in a DECLARE statement should be an expression
-- src: https://dev.mysql.com/doc/refman/5.7/en/declare-local-variable.html
-- delimiter //
CREATE PROCEDURE procedure1()
BEGIN
  DECLARE var1 INT unsigned default 1;
  DECLARE var2 TIMESTAMP default CURRENT_TIMESTAMP;
  DECLARE var3 INT unsigned default 2 + var1;
END -- //-- delimiter ;
#end
#begin
CREATE DEFINER=`system_user`@`%` PROCEDURE `update_order`(IN orderID bigint(11))
BEGIN  insert into order_config(order_id, attribute, value, performer)
       SELECT orderID, 'first_attr', 'true', 'AppConfig'
       WHERE NOT EXISTS (select 1 from inventory.order_config t1 where t1.order_id = orderID and t1.attribute = 'first_attr') OR
             EXISTS (select 1 from inventory.order_config p2 where p2.order_id = orderID and p2.attribute = 'first_attr' and p2.performer = 'AppConfig')
       ON DUPLICATE KEY UPDATE value = 'true',
                            performer = 'AppConfig'; -- Enable second_attr for order
END
#end
#begin
CREATE DEFINER=`bettingservice`@`stage-us-nj-app%` PROCEDURE `AggregatePlayerFactDaily`()
BEGIN
    DECLARE CID_min BIGINT;
    DECLARE CID_max BIGINT;

    DECLARE EXIT HANDLER FOR SQLEXCEPTION
BEGIN
    SHOW ERRORS;
ROLLBACK;
END;

SELECT LastID+1 INTO CID_min FROM AggregateStatus
WHERE TableName = 'Combination_Transaction_Player_Fact';
SELECT Id INTO CID_max FROM Combination_Transaction ORDER BY Id DESC LIMIT 1;

START TRANSACTION;
UPDATE AggregateStatus SET LastId = CID_max, LastUpdated = CURRENT_TIMESTAMP
WHERE TableName = 'Combination_Transaction_Player_Fact';

INSERT INTO Combination_Transaction_Player_Fact
SELECT
    NULL `Id`,
    CT.Player_UID,
    CT.Tx_Type `Type`,
    DATE(BT.Timestamp) `Date`,
    SUM(CT.Real_Amount) `Real_Amount`,
    SUM(CT.Bonus_Amount) `Bonus_Amount`,
    BT.Currency_UID,
    COUNT(CT.Id) Tx_Count,
    SUM(IF(CT.Real_Amount>0,1,0)) `Real_Count`,
    SUM(IF(CT.Bonus_Amount>0,1,0)) `Bonus_Count`
FROM Combination_Transaction CT
    LEFT JOIN Betting_Transaction BT ON CT.Betting_Tx_ID = BT.ID
WHERE CT.Id BETWEEN CID_min
  AND CID_max
GROUP BY CT.Player_UID, CT.Tx_Type, DATE(BT.Timestamp)
ON DUPLICATE KEY UPDATE
                     Currency_UID = VALUES(Currency_UID),
                     Tx_Count     = Tx_Count + VALUES(Tx_Count),
                     Real_Amount  = Real_Amount + VALUES(Real_Amount),
                     Bonus_Amount = Bonus_Amount + VALUES(Bonus_Amount),
                     Real_Count   = Real_Count + VALUES(Real_Count),
                     Bonus_Count  = Bonus_Count + VALUES(Bonus_Count);
COMMIT;
END
#end
-- Create procedure
-- delimiter //
CREATE PROCEDURE makesignal(p1 INT)
BEGIN
  DECLARE error_text VARCHAR(255);
  IF (error_text != 'OK') THEN
    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = error_text;
  END IF;
END -- //-- delimiter ;
#end
#begin
-- delimiter //
CREATE PROCEDURE set_unique_check()
BEGIN
    SET unique_checks=on;
END; -- //-- delimiter ;
#end
#begin
-- Create Role
create role 'RL_COMPLIANCE_NSA';
create role if not exists 'RL_COMPLIANCE_NSA';
CREATE ROLE 'admin', 'developer';
CREATE ROLE 'webapp'@'localhost';
#end
#begin
CREATE VIEW view_with_cte1 AS
WITH cte1 AS
(
    SELECT column_1 AS a, column_2 AS b
    FROM table1
)
SELECT a, b FROM cte1;
#end
#begin
CREATE VIEW view_with_cte2 AS
WITH cte1 (col1, col2) AS
(
  SELECT 1, 2
  UNION ALL
  SELECT 3, 4
),
cte2 (col1, col2) AS
(
  SELECT 5, 6
  UNION ALL
  SELECT 7, 8
)
SELECT col1, col2 FROM cte;
#end
#begin
CREATE VIEW view_with_cte3 AS
WITH cte (col1, col2) AS
(
  SELECT 1, 2
  UNION ALL
  SELECT 3, 4
)
SELECT col1, col2 FROM cte;
#end
#begin
CREATE VIEW view_with_cte4 AS
WITH RECURSIVE cte (n) AS
(
  SELECT 1
  UNION ALL
  SELECT n + 1 FROM cte WHERE n < 5
)
SELECT * FROM cte;
#end
