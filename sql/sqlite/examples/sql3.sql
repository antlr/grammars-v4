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
