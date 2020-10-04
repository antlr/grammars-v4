Select Name, printf('%,d',Bytes) Size,
    FIRST_VALUE(Name) OVER (
        ORDER BY Bytes
    ) AS SmallestTrack
FROM
    tracks
WHERE
    AlbumId = 1;
