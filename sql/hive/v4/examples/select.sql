WITH A AS (
    SELECT *, 1e1 as c, 1E1 as d, 1.0 as e FROM TB
)
SELECT A,B,C
FROM A

---column supports time keyword
SELECT time from A;

---column supports of keyword
SELECT of from A;

---column supports date keyword
SELECT date from A;

---column supports user keyword
SELECT user from A;