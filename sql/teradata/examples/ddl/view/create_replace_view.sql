-- Creating a View with Column Titles
CREATE VIEW dept AS
SELECT deptno(TITLE 'Department Number'),
       deptname(TITLE 'Department Name'),
       loc (TITLE 'Department Location'),
       mgrno(TITLE 'Manager Number')
FROM department;

-- Creating view with check option
CREATE VIEW staff_info (num, name, pos, department, sex, dob)
    AS
SELECT employee.empno, name, jobtitle, deptno, sex, dob
FROM employee
WHERE jobtitle NOT IN ('President', 'Supervisor')
WITH CHECK OPTION;

-- Creating a View With a Dynamic UDT Expression
CREATE VIEW multitype_v AS
SELECT udf_aggregate_mp_struc(NEW VARIANT_TYPE(MultiType.w AS w,
        MultiType.x AS x,
        MultiType.y AS y,
        NEW MP_STRUCTURED_INT(MultiType.w, MultiType.x,
        MultiType.y) AS z)) AS m
FROM MultiType;

-- Using a WITH Modifier in CREATE VIEW
CREATE VIEW sales_v AS
WITH multiple_orders AS (
    SELECT customer_id, COUNT(*) AS order_count
    FROM orders
    GROUP BY customer_id
    HAVING COUNT(*) > 1
),
multiple_order_totals AS (
    SELECT customer_id, SUM(total_cost) AS total_spend
    FROM orders
    WHERE customer_id IN (SELECT customer_id FROM multiple_orders)
    GROUP BY customer_id
)
SELECT * FROM multiple_order_totals;

-- Using a Recursive Query in a WITH Modifier in CREATE VIEW
CREATE VIEW t1_view
    AS
WITH RECURSIVE s5 (MinVersion_view) AS (SELECT a1 FROM
        t1 WHERE a1 > 1
        UNION ALL
        SEL MinVersion_view FROM s5 WHERE MinVersion_view > 3),
        RECURSIVE s6 (MinVersion_view2) AS (SELECT a1 FROM t1 WHERE a1 = 2
        UNION ALL
        SEL MinVersion_view2 FROM S6 WHERE MinVersion_view2 > 2 )
SEL * FROM s5, s6;
