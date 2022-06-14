HA$PBExportHeader$w_misth_zpepidom_list.srw
$PBExportComments$
forward
global type w_misth_zpepidom_list from w_list
end type
end forward

global type w_misth_zpepidom_list from w_list
integer width = 3250
integer height = 2200
string title = "title"
string menuname = "m_misth_zpepidom_list"
string icon = "res\pinakes.ico"
string is_tablename = "misth_zpepidom"
string is_order = " order by descepidom asc "
string is_formwin = "w_misth_zpepidom_form"
boolean ib_editwithkey = true
boolean ib_retrieve = true
boolean ib_sort = true
boolean ib_noview = true
event me_create_yvar ( )
event me_create_krat ( )
end type
global w_misth_zpepidom_list w_misth_zpepidom_list

forward prototypes
public subroutine of_deleterow (ref datawindow adw, long row)
protected subroutine of_dw2struct (ref datawindow adw, long row)
protected subroutine of_struct2dw (ref datawindow adw, long row)
protected subroutine of_init_struct ()
protected subroutine of_reset_struct ()
protected subroutine of_retrieve (ref datawindow adw)
end prototypes

event me_create_yvar();// Äçìéïõñãßá ìåôáâëçôÞò õðáëëÞëïõ

// Ðáßñíïõìå ôçí åðéëåãìÝíç åããñáöÞ
	long		ll_row
	
	ll_row = dw.getrow()
	
	if ll_row = 0 then return
	
// Ðáßñíïõìå ôïí êùäéêü êáé ôçí ðåñéãñáöÞ ôïõ åðéäüìáôïò
// êáé áíïßãïõìå ôç öüñìá äçìïõñãßáò ìåôáâëçôÞò õðáëëÞëïõ

	gsc_misth_zpyvar_reset()
	
	gsc_misth_zpyvar.kodyvar = dw.object.kodepidom[ll_row]
	gsc_misth_zpyvar.kodxrisi = gs_kodxrisi
	gsc_misth_zpyvar.descyvar = dw.object.descepidom[ll_row]

	OpenWithParm(w_misth_zpyvar_form, 0)
	
end event

event me_create_krat();// Äçìéïõñãßá êñÜôçóçò


// Ðáßñíïõìå ôçí åðéëåãìÝíç åããñáöÞ
	long		ll_row
	
	ll_row = dw.getrow()
	
	if ll_row = 0 then return
	
// Ðáßñíïõìå ôïí êùäéêü êáé ôçí ðåñéãñáöÞ ôçò óôáèåñÜò
// êáé áíïßãïõìå ôç öüñìá äçìïõñãßáò êñÜôçóçò

	gsc_misth_zpkrat_reset()
	
	gsc_misth_zpkrat.kodkrat = dw.object.kodepidom[ll_row]
	gsc_misth_zpkrat.kodxrisi = gs_kodxrisi
	gsc_misth_zpkrat.desckrat = dw.object.descepidom[ll_row]

	OpenWithParm(w_misth_zpkrat_form, 0)
	
end event

public subroutine of_deleterow (ref datawindow adw, long row);string	ls_kodepidom

ls_kodepidom = adw.object.kodepidom[row]

delete from misth_zpepidom
where kodepidom = :ls_kodepidom and kodxrisi = :gs_kodxrisi;
fn_sqlerror()
commit;
end subroutine

protected subroutine of_dw2struct (ref datawindow adw, long row);gsc_misth_zpepidom.kodepidom = adw.object.kodepidom[row]
gsc_misth_zpepidom.kodxrisi = adw.object.kodxrisi[row]
gsc_misth_zpepidom.descepidom = adw.object.descepidom[row]
gsc_misth_zpepidom.hasforo = adw.object.hasforo[row]
gsc_misth_zpepidom.expr = adw.object.expr[row]
gsc_misth_zpepidom.isasf = adw.object.isasf[row]
gsc_misth_zpepidom.autoforos = adw.object.autoforos[row]
gsc_misth_zpepidom.hasasf = adw.object.hasasf[row]

end subroutine

protected subroutine of_struct2dw (ref datawindow adw, long row);adw.object.kodepidom[row] = gsc_misth_zpepidom.kodepidom
adw.object.kodxrisi[row] = gsc_misth_zpepidom.kodxrisi
adw.object.descepidom[row] = gsc_misth_zpepidom.descepidom
adw.object.hasforo[row] = gsc_misth_zpepidom.hasforo
adw.object.expr[row] = gsc_misth_zpepidom.expr
adw.object.isasf[row] = gsc_misth_zpepidom.isasf
adw.object.autoforos[row] = gsc_misth_zpepidom.autoforos
adw.object.hasasf[row] = gsc_misth_zpepidom.hasasf
end subroutine

protected subroutine of_init_struct ();gsc_misth_zpepidom.kodxrisi =  gs_kodxrisi
gsc_misth_zpepidom.hasforo = 0 
gsc_misth_zpepidom.isasf = 0 
gsc_misth_zpepidom.autoforos = 0 
gsc_misth_zpepidom.hasasf = 0 

end subroutine

protected subroutine of_reset_struct ();gsc_misth_zpepidom_reset()
end subroutine

protected subroutine of_retrieve (ref datawindow adw);dw.retrieve(gs_kodxrisi)
end subroutine

on w_misth_zpepidom_list.create
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_misth_zpepidom_list" then this.MenuID = create m_misth_zpepidom_list
end on

on w_misth_zpepidom_list.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
end on

event open;call super::open;// translation
	title = trn(95) + " - " + trn(306)
end event

type dw from w_list`dw within w_misth_zpepidom_list
integer width = 3214
integer height = 1496
string dataobject = "dw_misth_zpepidom_list"
end type

