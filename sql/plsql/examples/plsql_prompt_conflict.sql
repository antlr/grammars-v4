CREATE OR REPLACE PROCEDURE test_proc IS
CURSOR C_Pro IS
	SELECT 1 AS col FROM dual;

Pro C_Pro%ROWTYPE; -- 'Pro' should be a valid identifier, not a PROMPT command

BEGIN
    OPEN C_Pro;
    FETCH C_Pro INTO Pro;
    CLOSE C_Pro;
END;
/
