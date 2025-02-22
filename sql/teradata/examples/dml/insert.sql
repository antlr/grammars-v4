-- Inserting a Row
INSERT INTO employee (name, id, dept_id, dob, sex)
VALUES ('Angela F', 100, 12, date'1990-01-01', 'F');

-- Insert Using a SELECT Subquery
INSERT INTO bonus (dept_id, employee_name, year_id, amount)
SELECT dept_id, name, 2024, salary * 0.25
FROM employee
WHERE years_exp > 10 ;


-- Simple INSERT Requests Using a DEFAULT Function
INSERT INTO table10
VALUES (1, DEFAULT(col2), DEFAULT(col3));

-- Logging Errors with INSERT ... SELECT
INSERT INTO t
SELECT *
FROM s
LOGGING ERRORS WITH NO LIMIT;

-- HASH BY Option with NoPI Table
INSERT INTO orders
SELECT *
FROM pre_orders
HASH BY order_k
LOCAL ORDER BY order_ts;

-- Inserting JSON data
INSERT INTO MyTable JSON '{"pkey":123,"val":1234}';
