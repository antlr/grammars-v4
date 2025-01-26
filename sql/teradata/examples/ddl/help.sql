-- Help online
HELP 'SQL CREATE TABLE';

-- Help fully qualified column name
HELP COLUMN table_name.column_name, table_name.other_column;

-- Help column from
HELP COLUMN column_name FROM table_name;

HELP COLUMN employee.name, employee.dept_no, department.dept_no
FROM employee, department;

-- Help for all columns
HELP COLUMN * FROM table_name;

HELP COLUMN table_name.*, other_table.*;

-- Help column on mixture of non-column expressions and table_name.* expressions
HELP COLUMN 1+1, qnty * 100, tbl.*;

-- Help Error Table For data table
HELP ERROR TABLE FOR db.sales;

-- Help Index on table columns
HELP INDEX tname (cname_1, cname_2, cname_3);

-- Help Procedure
HELP PROCEDURE db.prc ATTRIBUTES;

-- Help Function with data types
HELP FUNCTION truncate_val (varchar) ;

-- Help Specific Function
HELP SPECIFIC FUNCTION truncate_str ;

-- Help Method
HELP METHOD int_ordering() FOR udt_int;

-- Help Specific Method
HELP SPECIFIC METHOD sysudtlib.circle;

-- HELP TYPE for a UDT with the ATTRIBUTE Option
HELP TYPE address ATTRIBUTE;

-- HELP storage_format SCHEMA
HELP Avro SCHEMA chemDatasetSchema;

-- HELP CAST source
HELP CAST SYSUDTLIB.euro SOURCE;

-- HELP FOREIGN SERVER
HELP FOREIGN SERVER QG_Presto1;

-- HELP FOREIGN DATABASE
HELP FOREIGN DATABASE "default"@QG_Presto1;

-- HELP FOREIGN TABLE
HELP FOREIGN TABLE TBL@QG_Presto1;

-- HELP FOREIGN FUNCTION
HELP FOREIGN FUNCTION syslib.MonitorQueryBand@QG_Presto1 ;
