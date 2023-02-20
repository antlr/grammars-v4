create procedure sp (@val numeric) as begin print @val end
go
exec sp $5
go
exec sp $-5
go
exec sp $+5
go
exec sp -$5
go
exec sp -$-5
go
exec sp +$-5 -- should fail
go
exec sp -1
go
exec sp - -1 -- should fail
go
exec sp +1 -- should fail
go
select $5
go
select $-5
go
select $+5
go
select -$5
go
select -$-5
go
select +$-5
go
select -1
go
select - -1
go
select +1
go
