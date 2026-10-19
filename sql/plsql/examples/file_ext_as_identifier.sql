-- trg / vw / pkb / pks / fnc / prc are ordinary identifiers.
-- SQL*Plus @, @@, and START accept the same words as a file suffix.

@foo.sql
@foo.pkb
@@bar.pks
start baz.trg

CREATE OR REPLACE TRIGGER trg
BEFORE INSERT ON my_table
FOR EACH ROW
BEGIN
    NULL;
END;
/

SELECT vw.col
FROM my_view vw
/

SELECT pkb.col
FROM t pkb
/

SELECT fnc.col
FROM t fnc
/

SELECT prc.col
FROM t prc
/
