SELECT * FROM foo WHERE id = 10;

SELECT * FROM foo WHERE id < 25;

SELECT * FROM foo WHERE id > 25;

SELECT * FROM foo WHERE id <= 25;

SELECT * FROM foo WHERE id >= 25;

SELECT * FROM foo WHERE id != 10;

SELECT * FROM foo where id IS 10;

SELECT * FROM foo where id IS NOT 25;

SELECT * FROM foo where id IS NOT DISTINCT FROM 10;

SELECT * FROM foo where id IS DISTINCT FROM 25;

SELECT * FROM foo where id IN (1, 2, 3);

SELECT * FROM foo where id = 10 AND id != 25;

SELECT * FROM foo where id = 10 OR id = 11;

SELECT '{"a": "b"}' -> 'a';

SELECT '{"a": "b"}' ->> 'a';

SELECT a FROM tb WHERE a BETWEEN 100 AND 200 AND b > 0;

SELECT 1 BETWEEN 2 AND 3 AND 4 AND 5;
