# Google Fusion Tables Sql

Parses Google fusion tables sql as defined in the
[Row and Query SQL Reference](https://developers.google.com/fusiontables/docs/v2/sql-reference)
as of January 2016. 

Augmented by expressions for

* `ALTER TABLE [table name or ID] RENAME TO [new name]`
* `DROP TABLE  [table name or ID]`
* `CREATE TABLE [table name] AS SELECT * FROM [source table name or ID]`

It's fallout from a related [query utility](https://github.com/curiosag/ftc)
intended to reduce the pain-in-the-ass factor of the whole issue,
if anyone is interested in such a thing.

# License

[The Unlicense](hoosealicense.com/licenses/unlicense/).