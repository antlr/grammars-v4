--https://learn.microsoft.com/en-US/sql/t-sql/statements/create-view-transact-sql?view=sql-server-2016
CREATE OR ALTER VIEW xyz_view
AS
SELECT
  ccc,
  xxx,
  yyy
  zzz
FROM xyz
;
GO

CREATE OR REPLACE VIEW xyz_view
AS
SELECT
    ccc,
    xxx,
    yyy
        zzz
FROM xyz
;
GO