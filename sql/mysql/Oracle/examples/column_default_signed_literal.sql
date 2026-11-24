-- A sign may precede a number in a column DEFAULT (signed_literal in sql_yacc.yy), not a hex or bit literal.
CREATE TABLE t_def (a INT DEFAULT -1, b DECIMAL(5,2) DEFAULT +1.5, c DOUBLE DEFAULT -1e3, d BIGINT DEFAULT -9223372036854775808, h VARBINARY(2) DEFAULT 0x0A);
ALTER TABLE t_def ALTER COLUMN a SET DEFAULT +2, ALTER COLUMN c SET DEFAULT -2.5e-1;
