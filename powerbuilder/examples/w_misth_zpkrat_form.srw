HA$PBExportHeader$w_misth_zpkrat_form.srw
$PBExportComments$
forward
global type w_misth_zpkrat_form from w_form
end type
type gb_1 from groupbox within w_misth_zpkrat_form
end type
end forward

global type w_misth_zpkrat_form from w_form
integer width = 2286
integer height = 696
string title = "title"
string icon = "res\pinakes.ico"
boolean ib_update = true
string is_tablename = "misth_zpkrat"
gb_1 gb_1
end type
global w_misth_zpkrat_form w_misth_zpkrat_form

type variables
string	is_kodkrat
end variables

forward prototypes
public function boolean of_check4required (ref datawindow adw, long row)
public subroutine of_dw2struct (ref datawindow adw, long row)
public subroutine of_struct2dw (ref datawindow adw, long row)
protected subroutine of_retrieve ()
protected subroutine of_storekey ()
end prototypes

public function boolean of_check4required (ref datawindow adw, long row);
string		lstring	
long		llong	
date		ldate
time		ltime
long		ll_count

// kodkrat
	lstring = adw.object.kodkrat[row]
	if isnull(lstring) or lstring = "" then
		Messagebox(gs_app_name, trn(188))
		adw.setfocus()
		adw.setcolumn("kodkrat")
		return false
	end if
	
	// ¸ëåã÷ïò áí ï êùäéêüò Ý÷åé êáôá÷ùñçèåß
		if lstring <> is_kodkrat or isnull(is_kodkrat) or is_kodkrat = "" then
			select count(kodkrat) into :ll_count from misth_zpkrat
			where kodkrat = :lstring and kodxrisi = :gs_kodxrisi;
			fn_sqlerror()
			if ll_count > 0 then
				Messagebox(gs_app_name, tr("Ï Êùäéêüò") + " '" + lstring + "' " + trn(658))
				adw.setfocus()
				adw.setcolumn("kodkrat")
				return false
			end if
		end if

// desckrat
	lstring = adw.object.desckrat[row]
	if isnull(lstring) or lstring = "" then
		Messagebox(gs_app_name, trn(175))
		adw.setfocus()
		adw.setcolumn("desckrat")
		return false
	end if	
	
// everything ok
	return true
end function

public subroutine of_dw2struct (ref datawindow adw, long row);gsc_misth_zpkrat.kodkrat = adw.object.kodkrat[row]
gsc_misth_zpkrat.kodxrisi = adw.object.kodxrisi[row]
gsc_misth_zpkrat.desckrat = adw.object.desckrat[row]
gsc_misth_zpkrat.isforos = adw.object.isforos[row]
gsc_misth_zpkrat.isasf = adw.object.isasf[row]
gsc_misth_zpkrat.isautoforos = adw.object.isautoforos[row]
end subroutine

public subroutine of_struct2dw (ref datawindow adw, long row);adw.object.kodkrat[row] = gsc_misth_zpkrat.kodkrat
adw.object.kodxrisi[row] = gsc_misth_zpkrat.kodxrisi
adw.object.desckrat[row] = gsc_misth_zpkrat.desckrat
adw.object.isforos[row] = gsc_misth_zpkrat.isforos
adw.object.isasf[row] = gsc_misth_zpkrat.isasf
adw.object.isautoforos[row] = gsc_misth_zpkrat.isautoforos
end subroutine

protected subroutine of_retrieve ();dw_main.retrieve(is_kodkrat, gs_kodxrisi)
end subroutine

protected subroutine of_storekey ();is_kodkrat = gsc_misth_zpkrat.kodkrat
end subroutine

on w_misth_zpkrat_form.create
int iCurrent
call super::create
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_1
end on

on w_misth_zpkrat_form.destroy
call super::destroy
destroy(this.gb_1)
end on

event open;call super::open;// translation
	title = trn(596)
	
end event

type cb_cancel from w_form`cb_cancel within w_misth_zpkrat_form
integer x = 1915
integer y = 468
end type

type cb_ok from w_form`cb_ok within w_misth_zpkrat_form
integer x = 1550
integer y = 468
end type

type dw_main from w_form`dw_main within w_misth_zpkrat_form
integer x = 64
integer y = 80
integer width = 2011
integer height = 320
string dataobject = "dw_misth_zpkrat_form"
end type

type gb_1 from groupbox within w_misth_zpkrat_form
integer x = 27
integer y = 4
integer width = 2199
integer height = 420
integer taborder = 10
integer textsize = -9
integer weight = 400
fontcharset fontcharset = greekcharset!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial Greek"
long textcolor = 33554432
long backcolor = 67108864
end type

