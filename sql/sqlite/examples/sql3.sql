Select Name, printf('%,d',Bytes) Size,
    FIRST_VALUE(Name) OVER (
        ORDER BY Bytes
    ) AS SmallestTrack
FROM
    tracks
WHERE
    AlbumId = 1;

-- Degenerate examples of window-defn without ORDER BY clause
SELECT * WINDOW window_name AS ();
SELECT row_number() OVER window_name FROM table_name WINDOW window_name AS ();

-- Allow ORDER BY in aggregate functions
SELECT group_concat(field_name_1, ',' ORDER BY field_name_2) FROM table_name;

SELECT UNBOUNDED FROM table_name;

-- NATURAL and other join operator keywords can only work as aliases if preceeded by AS
SELECT 1 FROM table_name AS NATURAL;
