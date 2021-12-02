HA$PBExportHeader$w_misth_ypal_search.srw
$PBExportComments$
forward
global type w_misth_ypal_search from w_searchex
end type
type gb_1 from groupbox within w_misth_ypal_search
end type
end forward

global type w_misth_ypal_search from w_searchex
integer width = 2309
integer height = 912
string icon = "res\ypal.ico"
gb_1 gb_1
end type
global w_misth_ypal_search w_misth_ypal_search

forward prototypes
protected function string of_createwhere ()
end prototypes

protected function string of_createwhere ();string	ls_where

string	lstring
long		llong	

// surname
	lstring = dw.object.surname[1]
	if not isnull(lstring) and lstring <> "" then
		ls_where = ls_where + " AND surname LIKE ~~'" + lstring + "%~~'"
	end if
	
// name
	lstring = dw.object.name[1]
	if not isnull(lstring) and lstring <> "" then
		ls_where = ls_where + " AND name LIKE ~~'" + lstring + "%~~'"
	end if
	
// fathername
	lstring = dw.object.fathername[1]
	if not isnull(lstring) and lstring <> "" then
		ls_where = ls_where + " AND fathername LIKE ~~'" + lstring + "%~~'"
	end if
	
// sex
	lstring = dw.object.sex[1]
	if not isnull(lstring) and lstring <> "" then
		ls_where = ls_where + " AND sex = ~~'" + lstring + "~~'"
	end if
	
// kodtmima
	lstring = dw.object.kodtmima[1]
	if not isnull(lstring) and lstring <> "" then
		ls_where = ls_where + " AND kodtmima = ~~'" + lstring + "~~'"
	end if
	
// kodidikot
	lstring = dw.object.kodidikot[1]
	if not isnull(lstring) and lstring <> "" then
		ls_where = ls_where + " AND kodidikot = ~~'" + lstring + "~~'"
	end if	
	
// mitroo
	lstring = dw.object.mitroo[1]
	if not isnull(lstring) and lstring <> "" then
		ls_where = ls_where + " AND mitroo = ~~'" + lstring + "~~'"
	end if	
	
// adt
	lstring = dw.object.adt[1]
	if not isnull(lstring) and lstring <> "" then
		ls_where = ls_where + " AND adt = ~~'" + lstring + "~~'"
	end if		
	
// afm
	lstring = dw.object.afm[1]
	if not isnull(lstring) and lstring <> "" then
		ls_where = ls_where + " AND afm = ~~'" + lstring + "~~'"
	end if	
	
// klimakio
	llong = dw.object.klimakio[1]
	if not isnull(llong) then
		ls_where = ls_where + " AND klimakio = " + string(llong)
	end if	

// klados
	lstring = dw.object.klados[1]
	if not isnull(lstring) and lstring <> "" then
		ls_where = ls_where + " AND klados = ~~'" + lstring + "~~'"
	end if		
	
// bathmos
	lstring = dw.object.bathmos[1]
	if not isnull(lstring) and lstring <> "" then
		ls_where = ls_where + " AND bathmos = ~~'" + lstring + "~~'"
	end if			

return ls_where



end function

on w_misth_ypal_search.create
int iCurrent
call super::create
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_1
end on

on w_misth_ypal_search.destroy
call super::destroy
destroy(this.gb_1)
end on

event open;call super::open;fn_retrievechild(dw, "kodtmima", gs_kodxrisi)
fn_retrievechild(dw, "kodidikot", gs_kodxrisi)

// translation
	dw.Object.sex.Values=trn(113) + "	1/" + trn(380) + "	" + "2/"
	title = trn(70)
	
end event

type cb_cancel from w_searchex`cb_cancel within w_misth_ypal_search
integer x = 1938
integer y = 680
end type

type cb_ok from w_searchex`cb_ok within w_misth_ypal_search
integer x = 1600
integer y = 680
end type

type dw from w_searchex`dw within w_misth_ypal_search
integer x = 69
integer y = 64
integer width = 2135
integer height = 544
string dataobject = "dw_misth_ypal_search"
end type

type gb_1 from groupbox within w_misth_ypal_search
integer x = 27
integer width = 2226
integer height = 640
integer taborder = 20
integer textsize = -9
integer weight = 400
fontcharset fontcharset = greekcharset!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial Greek"
long textcolor = 33554432
long backcolor = 67108864
end type

