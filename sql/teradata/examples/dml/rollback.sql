-- ROLLBACK with a UDT in the WHERE Clause
ROLLBACK WHERE (tab1.euro_col < CAST(0.0 AS euro));
ROLLBACK WHERE (tab1.cir_col.area() < 10.0);

-- Using an SQL UDF in the WHERE Clause of a ROLLBACK Request
ROLLBACK FROM t1
WHERE a1 = test.value_expression(2,3);
