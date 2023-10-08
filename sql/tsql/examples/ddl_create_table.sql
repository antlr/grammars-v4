-- create table with foreign key
CREATE TABLE dbo.Employee (
  EmployeeID int PRIMARY KEY CLUSTERED,
  SalesPersonID int NULL REFERENCES SalesPerson(SalesPersonID)
);
GO;

-- create table and with table constraint
CREATE TABLE dbo.Employee (
  EmployeeID int PRIMARY KEY CLUSTERED,
  ProductID int,
  SpecialOfferID int,
  CONSTRAINT FK_SpecialOfferProduct_SalesOrderDetail FOREIGN KEY (
    ProductID, SpecialOfferID)
  REFERENCES SpecialOfferProduct (
    ProductID, SpecialOfferID)
);
GO;

-- create table with multiple column constratints
CREATE TABLE [#TestTable] (
  [ID] INT PRIMARY KEY NOT NULL
) WITH (DATA_COMPRESSION = PAGE);
GO;

-- name these constratints
CREATE TABLE [#TestTable] (
  [ID] INT constraint pk PRIMARY KEY constraint notnull NOT NULL
) WITH (DATA_COMPRESSION = PAGE);
GO;

-- create table with multiple columns and constraints
CREATE TABLE [#TestTable] (
  [ID] INT PRIMARY KEY NOT NULL,
  [Name] NVARCHAR(64) NOT NULL
) WITH (DATA_COMPRESSION = PAGE);
GO;

-- create table with materialized column
create table xyz  (
  ccc  char(1),
  xxx  numeric(18,0) NULL,
  yyy  numeric(10,2) NULL,
  zzz  as cast(round(xxx / yyy, 0) as numeric(18,0)) materialized, 
  constraint pk primary key clustered (ccc)
)
go

-- non clustered index
create table #t (
    id varchar(50) not null index ix2 nonclustered,
    other_id uniqueidentifier not null index ix1 clustered,
    value float
)
go

-- table with partition scheme
CREATE TABLE dbo.PartitionTable (
    process_date datetime PRIMARY KEY,
    process_type char(10)
)
ON MyRangePS1 (process_date)
GO
