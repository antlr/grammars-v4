select 'A' | | 'B'  from dual;

SELECT JSON_OBJECT (
           KEY 'deptno' IS d.department_id,
           KEY 'deptname' IS d.department_name
    ) "Department Objects"
FROM departments d
ORDER BY d.department_id;

SELECT JSON_OBJECT(KEY 'VALUE' VALUE COL), t.*  FROM T t;

SELECT JSON_ARRAY (
               JSON_OBJECT('percentage' VALUE .50),
               JSON_ARRAY(1,2,3),
               100,
               'California',
               null
               NULL ON NULL
           ) "JSON Array Example"
FROM DUAL;

SELECT JSON_ARRAYAGG(id ORDER BY id RETURNING VARCHAR2(100)) ID_NUMBERS
FROM id_table;

SELECT JSON_OBJECTAGG(KEY department_name VALUE department_id) "Department Numbers"
FROM departments
WHERE department_id <= 30;

SELECT JSON_QUERY('{a:100, b:200, c:300}', '$') AS value
FROM DUAL;

SELECT JSON_SERIALIZE ('{a:[1,2,3,4]}' RETURNING VARCHAR2(3) TRUNCATE ERROR ON ERROR) from dual;

SELECT JSON_VALUE('{a:100}', '$.a') AS value FROM DUAL;



