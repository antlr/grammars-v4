HA$PBExportHeader$w_misth_zpkat_grid.srw
$PBExportComments$
forward
global type w_misth_zpkat_grid from w_pbgrid
end type
end forward

global type w_misth_zpkat_grid from w_pbgrid
integer width = 1861
integer height = 1572
string title = "title"
string icon = "res\pinakes.ico"
string is_tablename = "misth_zpkat"
end type
global w_misth_zpkat_grid w_misth_zpkat_grid

forward prototypes
public function long if_retrieve ()
public function boolean of_check4required (ref datawindow adw, long row)
public subroutine of_postinitrow (ref datawindow adw, long row)
public subroutine of_setrowcolors (ref long al_rowcolor, ref long al_rowrcolor)
end prototypes

public function long if_retrieve ();// Override if retrieval arguments required
// returns the number of rows
	
	long	ll_nrows
	
	ll_nrows = dw_main.retrieve(gs_kodxrisi)
	
	return ll_nrows
end function

public function boolean of_check4required (ref datawindow adw, long row);string	lstring	
long		ll_found

// kodkat
	lstring = adw.object.kodkat[row]
	if isnull(lstring) or lstring = "" then
		Messagebox(gs_app_name, trn(162))
		adw.setfocus()
		adw.setcolumn("kodkat")
		return false
	end if
	
	// ¸ëåã÷ïò áí ï êùäéêüò õðÜñ÷åé
		ll_found = adw.find("kodkat = '" + lstring + "'", 1, adw.rowcount())
		if ll_found = row then ll_found = adw.find("kodkat = '" + lstring + "'", ll_found + 1, adw.rowcount())
		if ll_found > 0 and ll_found <> row then
			MessageBox(gs_app_name, trn(133))
			adw.setfocus()
			adw.Setcolumn("kodkat")
			return false
		end if	

// desckat
	lstring = adw.object.desckat[row]
	if isnull(lstring) or lstring = "" then
		Messagebox(gs_app_name, trn(175))
		adw.setfocus()
		adw.setcolumn("desckat")
		return false
	end if
	
	// ¸ëåã÷ïò áí ç ðåñéãñáöÞ õðÜñ÷åé
		ll_found = adw.find("desckat = '" + lstring + "'", 1, adw.rowcount())
		if ll_found = row then ll_found = adw.find("desckat = '" + lstring + "'", ll_found + 1, adw.rowcount())
		if ll_found > 0 and ll_found <> row then
			MessageBox(gs_app_name, trn(130))
			adw.setfocus()
			adw.Setcolumn("desckat")
			return false
		end if	
	
	
// everything ok
	return true
end function

public subroutine of_postinitrow (ref datawindow adw, long row);adw.object.kodxrisi[row] = gs_kodxrisi
end subroutine

public subroutine of_setrowcolors (ref long al_rowcolor, ref long al_rowrcolor);al_rowrcolor = rgb(191,255,255)
end subroutine

on w_misth_zpkat_grid.create
call super::create
end on

on w_misth_zpkat_grid.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
end on

event open;call super::open;// translation
	title = trn(399)
	
end event

type dw_main from w_pbgrid`dw_main within w_misth_zpkat_grid
integer width = 1819
integer height = 1300
string dataobject = "dw_misth_zpkat_list"
end type

