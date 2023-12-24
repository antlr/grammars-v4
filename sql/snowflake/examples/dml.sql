create table tj(c int);
update tj as t set c = 1;
delete from tj as t;

MERGE INTO sch.dst as d
USING (
    SELECT 1 as c, 2 as c2, 3 as c3
) as s ON d.c = s.c
WHEN MATCHED AND s.c = 1 AND s.c2 = 1 THEN
UPDATE SET
    d.c = s.c,
    d.c2 = s.c2
WHEN MATCHED AND s.c != 1  AND s.c2 != 1 THEN
UPDATE SET
    d.c = s.c,
    d.c2 = s.c2,
    d.c3 = s.c3
WHEN NOT MATCHED THEN
INSERT (
    c,
    c2,
    c3
)
VALUES (
   s.c,
   s.c2,
   s.c3
);

insert into t(a,b)
select 1, 2
union
select 3, 4;

SELECT * FROM t, TABLE(flatten(
    INPUT => t.c
    , PATH => ''
    , OUTER => true
    , RECURSIVE => true
    , MODE => 'both'
));
