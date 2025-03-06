-- Granting Privileges to a Group of Users
GRANT SELECT
ON personnel.department
TO ALL finance;

-- Granting SELECT on Any Object
GRANT SELECT
ON personnel
TO moffit;

-- Granting SELECT on One Column of a Table
GRANT SELECT (column_1)
ON personnel.department
TO moffit;

-- Granting INSERT, UPDATE, REFERENCES on Some Columns of a Table
GRANT INSERT (id, name), UPDATE (name), REFERENCES(id)
ON sales.stock
TO supervisor;

-- Granting SELECT, INSERT, UPDATE, and DELETE on Any Object
GRANT SELECT, INSERT, UPDATE, DELETE
ON personnel
TO peterson;

-- Granting Privileges on an External Function
GRANT ALTER FUNCTION
ON SPECIFIC FUNCTION find_text
TO user_dba, user_in_house;

GRANT ALTER FUNCTION
ON syslib
TO user_dba;

GRANT CREATE FUNCTION
ON document_db
TO user_dba, user_in_house;

GRANT EXECUTE
ON FUNCTION text_process(CHAR,CHAR,INTEGER)
TO sammy;
