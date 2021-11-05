HA$PBExportHeader$w_misth_ypal_newklimakio.srw
$PBExportComments$
forward
global type w_misth_ypal_newklimakio from w_singleform
end type
end forward

global type w_misth_ypal_newklimakio from w_singleform
integer width = 2363
integer height = 1488
string icon = "res\ypal.ico"
end type
global w_misth_ypal_newklimakio w_misth_ypal_newklimakio

forward prototypes
public subroutine of_retrieve ()
end prototypes

public subroutine of_retrieve ();dw_main.retrieve(gs_kodxrisi, today())
end subroutine

on w_misth_ypal_newklimakio.create
call super::create
end on

on w_misth_ypal_newklimakio.destroy
call super::destroy
end on

event open;call super::open;title = trn(65)
cb_cancel.text = trn(28)
end event

type cb_cancel from w_singleform`cb_cancel within w_misth_ypal_newklimakio
integer x = 1015
integer y = 1256
end type

type cb_ok from w_singleform`cb_ok within w_misth_ypal_newklimakio
boolean visible = false
integer x = 1467
integer y = 1260
end type

type dw_main from w_singleform`dw_main within w_misth_ypal_newklimakio
integer x = 23
integer y = 24
integer width = 2295
integer height = 1176
string dataobject = "dw_misth_ypal_newklimakio"
end type

