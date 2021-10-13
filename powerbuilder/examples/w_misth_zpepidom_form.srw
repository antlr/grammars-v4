HA$PBExportHeader$w_misth_zpepidom_form.srw
$PBExportComments$
forward
global type w_misth_zpepidom_form from w_form
end type
type gb_1 from groupbox within w_misth_zpepidom_form
end type
end forward

global type w_misth_zpepidom_form from w_form
integer width = 2638
integer height = 1136
string title = "title"
string icon = "res\pinakes.ico"
boolean ib_update = true
string is_tablename = "misth_zpepidom"
gb_1 gb_1
end type
global w_misth_zpepidom_form w_misth_zpepidom_form

type variables
string	is_kodepidom
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

// kodepidom
	lstring = adw.object.kodepidom[row]
	if isnull(lstring) or lstring = "" then
		Messagebox(gs_app_name, trn(188))
		adw.setfocus()
		adw.setcolumn("kodepidom")
		return false
	end if
	
	// ¸ëåã÷ïò áí ï êùäéêüò Ý÷åé êáôá÷ùñçèåß
		if lstring <> is_kodepidom or isnull(is_kodepidom) or is_kodepidom = "" then
			select count(kodepidom) into :ll_count from misth_zpepidom
			where kodepidom = :lstring and kodxrisi = :gs_kodxrisi;
			fn_sqlerror()
			if ll_count > 0 then
				Messagebox(gs_app_name, trn(466) + " '" + lstring + "' " + trn(658))
				adw.setfocus()
				adw.setcolumn("kodepidom")
				return false
			end if
		end if

// descepidom
	lstring = adw.object.descepidom[row]
	if isnull(lstring) or lstring = "" then
		Messagebox(gs_app_name, trn(175))
		adw.setfocus()
		adw.setcolumn("descepidom")
		return false
	end if	
	
// everything ok
	return true
end function

public subroutine of_dw2struct (ref datawindow adw, long row);gsc_misth_zpepidom.kodepidom = adw.object.kodepidom[row]
gsc_misth_zpepidom.kodxrisi = adw.object.kodxrisi[row]
gsc_misth_zpepidom.descepidom = adw.object.descepidom[row]
gsc_misth_zpepidom.hasforo = adw.object.hasforo[row]
gsc_misth_zpepidom.expr = adw.object.expr[row]
gsc_misth_zpepidom.isasf = adw.object.isasf[row]
gsc_misth_zpepidom.autoforos = adw.object.autoforos[row]
gsc_misth_zpepidom.hasasf = adw.object.hasasf[row]

end subroutine

public subroutine of_struct2dw (ref datawindow adw, long row);adw.object.kodepidom[row] = gsc_misth_zpepidom.kodepidom
adw.object.kodxrisi[row] = gsc_misth_zpepidom.kodxrisi
adw.object.descepidom[row] = gsc_misth_zpepidom.descepidom
adw.object.hasforo[row] = gsc_misth_zpepidom.hasforo
adw.object.expr[row] = gsc_misth_zpepidom.expr
adw.object.isasf[row] = gsc_misth_zpepidom.isasf
adw.object.autoforos[row] = gsc_misth_zpepidom.autoforos
adw.object.hasasf[row] = gsc_misth_zpepidom.hasasf
end subroutine

protected subroutine of_retrieve ();dw_main.retrieve(is_kodepidom, gs_kodxrisi)
end subroutine

protected subroutine of_storekey ();is_kodepidom = gsc_misth_zpepidom.kodepidom
end subroutine

on w_misth_zpepidom_form.create
int iCurrent
call super::create
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_1
end on

on w_misth_zpepidom_form.destroy
call super::destroy
destroy(this.gb_1)
end on

event open;call super::open;// translation	
	title = trn(592)
	
end event

type cb_cancel from w_form`cb_cancel within w_misth_zpepidom_form
integer x = 2277
integer y = 184
end type

type cb_ok from w_form`cb_ok within w_misth_zpepidom_form
integer x = 2272
integer y = 40
end type

type dw_main from w_form`dw_main within w_misth_zpepidom_form
integer x = 64
integer y = 80
integer width = 2135
integer height = 932
string dataobject = "dw_misth_zpepidom_form"
end type

event dw_main::buttonclicked;call super::buttonclicked;this.accepttext()

choose case dwo.name
		
	case "b_expr"
		
		// ¢íïéãìá ôïõ w_expr ìå ôïí ôñÝ÷ïí ôýðï
			
			// Ðáßñíïõìå ôïí ôñÝ÷ïí ôýðï
				string		ls_expr 
				ls_expr = this.object.expr[row]
			
			// Öüñôùìá óôáèåñþí
				datastore		lds_stath
				lds_stath = fn_createds_zpstath()
				
			// Öüñôùìá üëùí ôùí ìåôáâëçôþí õðáëëÞëùí
				datastore		lds_yvar
				lds_yvar = fn_createds_zpyvar_all()
				
			// Öüñôùìá üëùí ôùí åðéäïìÜôùí
				datastore 		lds_epidom
				lds_epidom = fn_createds_zpepidom_all()
				
			// ÌåôáöïñÜ óå structure êáé Üíïéãìá w_expr
				s_expr	lsc_expr
				
				if lds_stath.rowcount() > 0 then
					lsc_expr.stath = lds_stath
				end if
			
				if lds_yvar.rowcount() > 0 then
					lsc_expr.yvar = lds_yvar
				end if
	
				if lds_epidom.rowcount() > 0 then
					lsc_expr.epidom = lds_epidom
				end if

				lsc_expr.expr = ls_expr
			
				
			// ¢íïéãìá w_expr
				integer	li_ret
				openwithparm(w_expr, lsc_expr)
				li_ret = message.doubleparm
				if li_ret <> 1 then return
				this.object.expr[row] = gstring
				
			// cleanup
				destroy lds_stath
				destroy lds_yvar
				destroy lds_epidom
						
					
			
		
end choose
end event

type gb_1 from groupbox within w_misth_zpepidom_form
integer x = 27
integer y = 4
integer width = 2199
integer height = 1020
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

