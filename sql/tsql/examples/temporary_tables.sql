-- Global temporary tables
CREATE PROCEDURE createMyGlobalTables  AS
   CREATE TABLE ##globalTemporary1
      (id INT DEFAULT 0);
   CREATE TABLE ##globalTemporary2
      (name VARCHAR(300) DEFAULT 'John');
   SELECT * FROM ##globalTemporary1;
   SELECT * FROM ##globalTemporary2;
   DROP TABLE ##globalTemporary1;
   DROP TABLE ##globalTemporary2;

GO
-- Local temporary tables
CREATE PROCEDURE createMyLocalTables  AS
    CREATE table #Color(
    Color varchar(10) PRIMARY key)
    INSERT INTO #color SELECT 'Red' UNION SELECT 'White'
     UNION SELECT 'green'UNION SELECT'Yellow'UNION SELECT'blue'
    DROP TABLE #color
    go
    CREATE table #Color(
    Color varchar(10) PRIMARY key)
    INSERT INTO #color SELECT 'Red' UNION SELECT 'White'
     UNION SELECT 'green'UNION SELECT'Yellow'UNION SELECT'blue'
    DROP TABLE #color
