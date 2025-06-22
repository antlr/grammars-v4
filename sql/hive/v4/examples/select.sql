WITH A AS (
    SELECT *, 1e1 as c, 1E1 as d, 1.0 as e FROM TB
)
SELECT A,B,C
FROM A
