SELECT TRY_PARSE('Jabberwokkie' AS datetime2 USING 'en-US') AS Result;

SELECT
    CASE WHEN TRY_PARSE('Aragorn' AS decimal USING 'sr-Latn-CS') IS NULL
        THEN 'True'
        ELSE 'False'
END
AS Result;


SET LANGUAGE English;
SELECT IIF(TRY_PARSE('01/01/2011' AS datetime2) IS NULL, 'True', 'False') AS Result;
