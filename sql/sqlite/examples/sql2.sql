CREATE TABLE f
(
    id INT PRIMARY KEY NOT NULL,
    i  TEXT            NOT NULL,
    j  INT             NOT NULL,
    k  CHAR(50),
    m  REAL
);

CREATE TABLE f
(
    id INT PRIMARY KEY NOT NULL,
    i  TEXT            NOT NULL,
    j  INT             NOT NULL,
    k  CHAR(50),
    m  REAL
) WITHOUT ROWID;

CREATE TABLE f
(
    id INT PRIMARY KEY NOT NULL,
    i  TEXT            NOT NULL,
    j  INT             NOT NULL,
    k  CHAR(50),
    m  REAL
) STRICT;

-- Module arguments can be any valid tokens with balanced parentheses (https://sqlite.org/lang_createvtab.html)
CREATE VIRTUAL TABLE f USING module_name(abc()d(te(s)t)f,gh`!`j%$m@p);
CREATE VIRTUAL TABLE f USING module_name(column VARCHAR(1,2), secondarg);
CREATE VIRTUAL TABLE f USING module_name(,,);

-- All identifiers are keywords (https://sqlite.org/c3ref/keyword_check.html)
CREATE TABLE BEGIN(REPLACE,PRAGMA,END);

-- Anything that can be parsed as column_constraint should be; only the remainder is parsed as type_name
CREATE TABLE test (name DEFAULT CURRENT_TIMESTAMP);
CREATE TABLE test (rowid DEFAULT CURRENT_TIMESTAMP);
CREATE TABLE test (name DOUBLE PRECISION PRIMARY KEY);
CREATE TABLE test (name CHECK (1));
CREATE TABLE test (name CONSTRAINT name CHECK (1));
CREATE TABLE test (name INT CONSTRAINT name CHECK (1));
