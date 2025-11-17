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

CREATE VIRTUAL TABLE f USING module_name(abc()d(te(s)t)f,gh!j%$m@p);
