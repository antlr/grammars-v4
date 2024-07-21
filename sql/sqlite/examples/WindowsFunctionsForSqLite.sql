SELECT
    Name,
    printf('%,d',Bytes) Size,
    FIRST_VALUE(Name) OVER (
        ORDER BY Bytes
    ) AS SmallestTrack
FROM
    tracks 
WHERE
    AlbumId = 1;

SELECT
    AlbumId,
    Name,
    printf('%,d',Bytes) Size,
    FIRST_VALUE(Name) OVER (
        PARTITION BY AlbumId
        ORDER BY Bytes DESC
		ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING
    ) AS LargestTrack
FROM
    tracks;
select * from COMPANY;

SELECT C.ID, C.NAME, C.AGE, D.DEPT
        FROM COMPANY AS C, DEPARTMENT AS D
        WHERE  C.ID = D.EMP_ID;

SELECT C.ID AS COMPANY_ID, C.NAME AS COMPANY_NAME, C.AGE, D.DEPT
        FROM COMPANY AS C, DEPARTMENT AS D
        WHERE  C.ID = D.EMP_ID;

SELECT 
	Value, 
	CUME_DIST() 
	OVER (
		ORDER BY value
	) CumulativeDistribution
FROM
	CumeDistDemo;
SELECT
	Val,
	DENSE_RANK () OVER ( 
		ORDER BY Val ) 
	ValRank 
FROM
	DenseRankDemo;


SELECT
	CustomerId,
	Year,
	Total,
	LAG ( Total, 1, 0 ) OVER ( 
		ORDER BY Year 
	) PreviousYearTotal 
FROM
	CustomerInvoices 
WHERE
	CustomerId = 4;

SELECT
	CustomerId,
	Year,
	Total,
	LAG ( Total,1,0) OVER ( 
		PARTITION BY CustomerId
		ORDER BY Year ) PreviousYearTotal 
FROM
	CustomerInvoices;


SELECT
    Name,
    printf ( '%.f minutes', 
                    Milliseconds / 1000 / 60 ) 
                    AS Length,
    LAST_VALUE ( Name ) OVER (
        ORDER BY Milliseconds 
        RANGE BETWEEN UNBOUNDED PRECEDING AND 
        UNBOUNDED FOLLOWING
    ) AS LongestTrack 
FROM
    tracks 
WHERE
    AlbumId = 4;


SELECT
    AlbumId,
    Name,
    printf ( '%.f minutes', 
                    Milliseconds / 1000 / 60 ) 
                    AS Length,
    LAST_VALUE ( Name ) OVER (
        PARTITION BY AlbumId
        ORDER BY Milliseconds DESC
        RANGE BETWEEN UNBOUNDED PRECEDING AND 
        UNBOUNDED FOLLOWING
    ) AS ShortestTrack 
FROM
    tracks;
	
SELECT
	CustomerId,
	Year,
	Total,
	LEAD ( Total,1,0) OVER ( ORDER BY Year ) NextYearTotal
FROM
	CustomerInvoices 
WHERE
	CustomerId = 1;

SELECT
	CustomerId,
	Year,
	Total,
	LEAD ( Total, 1, 0 ) OVER (
		PARTITION BY CustomerId 
		ORDER BY Year 
	) NextYearTotal 
FROM
	CustomerInvoices;

SELECT
    AlbumId,
    Name,
    Milliseconds Length,
    NTH_VALUE ( Name,2 ) OVER (
        PARTITION BY AlbumId
        ORDER BY Milliseconds DESC
        RANGE BETWEEN 
            UNBOUNDED PRECEDING AND 
            UNBOUNDED FOLLOWING
    ) AS SecondLongestTrack 
FROM
    tracks;
	
SELECT
    Name,
    Milliseconds Length,
    NTH_VALUE(name,2) OVER (
        ORDER BY Milliseconds DESC
    ) SecondLongestTrack
FROM
    tracks;
	
SELECT
	Name,
	Milliseconds,
	NTILE ( 4 ) OVER ( 
		ORDER BY Milliseconds ) LengthBucket
FROM
	tracks 
WHERE
	AlbumId = 1;
	
SELECT
	AlbumId,
	Name,
	Milliseconds,
	NTILE ( 3 ) OVER ( 
		PARTITION BY AlbumId
		ORDER BY Bytes ) SizeBucket
FROM
	tracks;

SELECT
    Name,
    Milliseconds,
    PERCENT_RANK() OVER( 
        ORDER BY Milliseconds 
    ) LengthPercentRank
FROM
    tracks 
WHERE
    AlbumId = 1;
	
SELECT
    Name,
    Milliseconds,
    printf('%.2f',PERCENT_RANK() OVER( 
        ORDER BY Milliseconds 
    )) LengthPercentRank
FROM
    tracks 
WHERE
    AlbumId = 1;

SELECT
    AlbumId,
    Name,
    Bytes,
    printf('%.2f',PERCENT_RANK() OVER( 
        PARTITION BY AlbumId
        ORDER BY Bytes 
    )) SizePercentRank
FROM
    tracks;
	
SELECT
	Val,
	RANK () OVER ( 
		ORDER BY Val 
	) ValRank
FROM
	RankDemo;

SELECT
	Name,
	Milliseconds,
	RANK () OVER ( 
		ORDER BY Milliseconds DESC
	) LengthRank 
FROM
	tracks;

SELECT
	Name,
	Milliseconds,
	AlbumId,
	RANK () OVER ( 
		PARTITION BY AlbumId
		ORDER BY Milliseconds DESC
	) LengthRank 
FROM
	tracks;
	
	
SELECT 
	* 
FROM (
	SELECT
		Name,
		Milliseconds,
		AlbumId,
		RANK () OVER ( 
			PARTITION BY AlbumId
			ORDER BY Milliseconds DESC
		) LengthRank 
	FROM
		tracks
) 
WHERE 
	LengthRank = 2;
	
SELECT
    ROW_NUMBER () OVER ( 
        ORDER BY Country 
    ) RowNum,
    FirstName,
    LastName,
    country 
FROM
    customers;

SELECT
    ROW_NUMBER () OVER ( 
        PARTITION BY Country
        ORDER BY FirstName
    ) RowNum,
    FirstName,
    LastName,
    country 
FROM
    customers;

SELECT * FROM (
    SELECT
        ROW_NUMBER () OVER ( 
            ORDER BY FirstName
        ) RowNum,
        FirstName,
        LastName,
        Country 
    FROM
        customers
) t
WHERE 
    RowNum > 20 AND RowNum <= 30;


SELECT 
    Country,
    FirstName,
    LastName,
    Amount
FROM (
    SELECT 
        Country, 
        FirstName,
        LastName,
        Amount,
        ROW_NUMBER() OVER (
            PARTITION BY country 
            ORDER BY Amount DESC
        ) RowNum
    FROM 
        Sales )
WHERE
    RowNum = 1;