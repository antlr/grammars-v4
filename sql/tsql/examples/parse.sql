SELECT PARSE('Monday, 13 December 2010' AS datetime2 USING 'en-US') AS Result;

SELECT PARSE('€345,98' AS money USING 'de-DE') AS Result;

-- The English language is mapped to en-US specific culture  
SET LANGUAGE 'English';
SELECT PARSE('12/16/2010' AS datetime2) AS Result;
