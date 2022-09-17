USE tempdb

CREATE TABLE col_elem_ex
(
	ident int NOT NULL IDENTITY(1,1),
	rguid UNIQUEIDENTIFIER NOT NULL DEFAULT NEWID() ROWGUIDCOL,
	col1 INT
)

INSERT INTO col_elem_ex(col1) VALUES(2)

SELECT $IDENTITY AS ident_col FROM col_elem_ex

SELECT $ROWGUID AS guid_col FROM col_elem_ex

SELECT col_elem_ex.col1 AS qualified_col FROM col_elem_ex

SELECT col1 AS nonQualified_col FROM col_elem_ex

SELECT NULL AS null_col FROM col_elem_ex

DROP TABLE col_elem_ex
