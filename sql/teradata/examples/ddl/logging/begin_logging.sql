-- Using BEGIN LOGGING
BEGIN LOGGING WITH TEXT
ON EACH USER, DATABASE, GRANT;

-- Logging UDTs with SQL Text
BEGIN LOGGING DENIALS WITH TEXT
ON EACH UDTTYPE;

-- Logging on First UDT Method with SQL Text
BEGIN LOGGING WITH TEXT
ON FIRST UDTMETHOD
ON DATABASE SYSUDTLIB;

-- Logging UDTs in SYSUDTLIB
BEGIN LOGGING DENIALS
ON EACH UDTUSAGE
ON DATABASE SYSUDTLIB;

-- Column-Level Discretionary Access Control and Row-Level Security Audit Logging
BEGIN LOGGING DENIALS WITH TEXT
ON EACH INSERT BY ivanov_ii
ON TABLE hr.emp_record;

BEGIN LOGGING DENIALS
ON EACH OVERRIDE INSERT
FOR CONSTRAINT group_membership
ON TABLE securedb.emp_record;

-- Logging Denials for a Row-Level Security Constraint
BEGIN LOGGING DENIALS
ON EACH OVERRIDE SELECT
FOR CONSTRAINT classification_category;

-- Logging the Denied First Insert Into a Row-Level Security- Secured Table
BEGIN LOGGING DENIALS
ON FIRST OVERRIDE INSERT
FOR CONSTRAINT classification_level
ON TABLE secure_db.emp_record;
