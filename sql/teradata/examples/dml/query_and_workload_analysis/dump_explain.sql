-- DUMP EXPLAIN Request With Null Query Plan Nam
DUMP EXPLAIN INTO TLE_queries
     SELECT id, address
     FROM employee
     WHERE name = 'Ivanov';

-- DUMP EXPLAIN Request With Query Plan Name
DUMP EXPLAIN INTO TLE_queries AS "Employee Smith Query"
     SELECT id, address
     FROM employee
     WHERE name = 'Ivanov';
