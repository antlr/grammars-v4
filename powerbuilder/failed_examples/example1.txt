$PBExportHeader$hellow.sra
$PBExportComments$Hello World Application
forward
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global type hellow from application
 end type
global hellow hellow

on open;messagebox("Hello World", "Hello World!")
end on
on hellow.create
appname = "hellow"
sqlca = create transaction
sqlda = create dynamicdescriptionarea
sqlsa = create dynamicstagingarea
error = create error
message = create message
end on

on hellow.destroy
destroy( sqlca )
destroy( sqlda )
destroy( sqlsa )
destroy( error )
destroy( message )
end on
