HA$PBExportHeader$pfc_w_print.srw
$PBExportComments$PFC Print dialog
forward
global type pfc_w_print from w_response
end type
type cbx_printtofile from u_cbx within pfc_w_print
end type
type rb_all from u_rb within pfc_w_print
end type
type rb_pages from u_rb within pfc_w_print
end type
type st_1 from u_st within pfc_w_print
end type
type em_frompage from u_em within pfc_w_print
end type
type st_2 from u_st within pfc_w_print
end type
type em_topage from u_em within pfc_w_print
end type
type rb_selection from u_rb within pfc_w_print
end type
type p_collate from u_p within pfc_w_print
end type
type gb_1 from u_gb within pfc_w_print
end type
type st_3 from u_st within pfc_w_print
end type
type em_copies from u_em within pfc_w_print
end type
type cbx_collate from u_cbx within pfc_w_print
end type
type cb_ok from u_cb within pfc_w_print
end type
type cb_cancel from u_cb within pfc_w_print
end type
type cb_printer from u_cb within pfc_w_print
end type
type gb_2 from u_gb within pfc_w_print
end type
end forward

global type pfc_w_print from w_response
integer x = 837
integer y = 752
integer width = 1984
integer height = 1172
string title = "Print"
long backcolor = 80263328
event pfc_printer ( )
cbx_printtofile cbx_printtofile
rb_all rb_all
rb_pages rb_pages
st_1 st_1
em_frompage em_frompage
st_2 st_2
em_topage em_topage
rb_selection rb_selection
p_collate p_collate
gb_1 gb_1
st_3 st_3
em_copies em_copies
cbx_collate cbx_collate
cb_ok cb_ok
cb_cancel cb_cancel
cb_printer cb_printer
gb_2 gb_2
end type
global pfc_w_print pfc_w_print

type variables
protected:
long	il_minpage = 1
long	il_maxpage = 65535
s_printdlgattrib	istr_printdlg
end variables

event pfc_printer;call super::pfc_printer;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  pfc_printer
//
//	Arguments:  none
//
//	Returns:  none
//
//	Description:
//	Printer setup
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	5.0.02   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright $$HEX2$$a9002000$$ENDHEX$$1996-1997 Sybase, Inc. and its subsidiaries.  All rights reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

if PrintSetup() = -1 then
	of_MessageBox ("pfc_print_accesserror", "Print", &
		"An error has occurred accessing printer setup", exclamation!, Ok!, 1)
end if
end event

on pfc_w_print.create
int iCurrent
call super::create
this.cbx_printtofile=create cbx_printtofile
this.rb_all=create rb_all
this.rb_pages=create rb_pages
this.st_1=create st_1
this.em_frompage=create em_frompage
this.st_2=create st_2
this.em_topage=create em_topage
this.rb_selection=create rb_selection
this.p_collate=create p_collate
this.gb_1=create gb_1
this.st_3=create st_3
this.em_copies=create em_copies
this.cbx_collate=create cbx_collate
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.cb_printer=create cb_printer
this.gb_2=create gb_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cbx_printtofile
this.Control[iCurrent+2]=this.rb_all
this.Control[iCurrent+3]=this.rb_pages
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.em_frompage
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.em_topage
this.Control[iCurrent+8]=this.rb_selection
this.Control[iCurrent+9]=this.p_collate
this.Control[iCurrent+10]=this.gb_1
this.Control[iCurrent+11]=this.st_3
this.Control[iCurrent+12]=this.em_copies
this.Control[iCurrent+13]=this.cbx_collate
this.Control[iCurrent+14]=this.cb_ok
this.Control[iCurrent+15]=this.cb_cancel
this.Control[iCurrent+16]=this.cb_printer
this.Control[iCurrent+17]=this.gb_2
end on

on pfc_w_print.destroy
call super::destroy
destroy(this.cbx_printtofile)
destroy(this.rb_all)
destroy(this.rb_pages)
destroy(this.st_1)
destroy(this.em_frompage)
destroy(this.st_2)
destroy(this.em_topage)
destroy(this.rb_selection)
destroy(this.p_collate)
destroy(this.gb_1)
destroy(this.st_3)
destroy(this.em_copies)
destroy(this.cbx_collate)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.cb_printer)
destroy(this.gb_2)
end on

event open;call super::open;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  open
//
//	Description:  Initialize window from passed in printdlg structure
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	5.0.02   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright $$HEX2$$a9002000$$ENDHEX$$1996-1997 Sybase, Inc. and its subsidiaries.  All rights reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

string	ls_frompage

ib_disableclosequery = true

istr_printdlg = message.powerobjectparm

// Check appropriate page range selection
rb_all.checked = istr_printdlg.b_allpages
rb_pages.checked = istr_printdlg.b_pagenums
rb_selection.checked = istr_printdlg.b_selection

// Disable page numbers option if requested
if istr_printdlg.b_disablepagenums then
	rb_pages.enabled = false
	em_frompage.enabled = false
	em_topage.enabled = false
end if

// Disable page selection if requested
rb_selection.enabled = not istr_printdlg.b_disableselection

// Collate
cbx_collate.checked = istr_printdlg.b_collate
if cbx_collate.checked then
	p_collate.picturename = "collat.bmp"
end if

// Copies
if istr_printdlg.l_copies < 1 then
	istr_printdlg.l_copies = 1
end if
em_copies.text = String (istr_printdlg.l_copies)

// Print to file
cbx_printtofile.checked = istr_printdlg.b_printtofile
cbx_printtofile.enabled = not istr_printdlg.b_disableprinttofile
cbx_printtofile.visible = not istr_printdlg.b_hideprinttofile

// Page range
ls_frompage = String (istr_printdlg.l_frompage)
em_frompage.text = ls_frompage
em_topage.text = String (istr_printdlg.l_topage)

// Calculate min and max page
il_minpage = istr_printdlg.l_minpage
if il_minpage < 1 then
	il_minpage = 1
end if
il_maxpage = istr_printdlg.l_maxpage
if il_maxpage > 65535 then
	il_maxpage = 65535
end if

if rb_pages.enabled then
	em_frompage.text = ls_frompage
end if

// Use the b_disablepagenums element to determine whether an action was taken
// for the dialog
SetNull (istr_printdlg.b_disablepagenums)
end event

event pfc_default;call super::pfc_default;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  pfc_default
//
//	Arguments:  none
//
//	Returns:  none
//
//	Description:
//	Populate printdlg structure based on user selections and return it
//	in the message object.
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	5.0.02   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright $$HEX2$$a9002000$$ENDHEX$$1996-1997 Sybase, Inc. and its subsidiaries.  All rights reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

integer	li_copies
long	ll_frompage
long	ll_topage
string	ls_invalidpagerange

ls_invalidpagerange = "This value is not in the page range.~r~n" + &
	"Please enter a number between " + String (il_minpage) + " and " + 	String (il_maxpage) + "."

ll_frompage = Long (em_frompage.text)
ll_topage = Long (em_topage.text)

// Validate page range
if rb_pages.enabled and rb_pages.checked then
	if ll_frompage < il_minpage or ll_frompage > il_maxpage then
		of_MessageBox ("pfc_print_invalidpagerange", "Print", ls_invalidpagerange, &
			exclamation!, Ok!, 1)
		em_frompage.SetFocus()
		return
	end if

	if ll_topage < il_minpage or ll_topage > il_maxpage then
		of_MessageBox ("pfc_print_invalidpagerange", "Print", ls_invalidpagerange, &
			exclamation!, Ok!, 1)
		em_topage.SetFocus()
		return
	end if

	if ll_frompage > ll_topage then
		of_MessageBox ("pfc_print_invalidfromvalue", "Print", &
			"The 'From' value cannot be greater than the 'To' value.", &
			exclamation!, Ok!, 1)
		em_frompage.SetFocus()
		return
	end if
end if

// Validate number of copies
li_copies = Integer (em_copies.text)
if li_copies <= 0 then
	of_MessageBox ("pfc_print_invalidcopies", "Print", &
		"Copies must be greater than zero", exclamation!, Ok!, 1)
	em_copies.SetFocus()
	return
end if

// Populate printdlg structure with values that the user selected
// from the print dialog
istr_printdlg.b_allpages = rb_all.checked
istr_printdlg.b_pagenums = rb_pages.checked
istr_printdlg.b_selection = rb_selection.checked
istr_printdlg.b_collate = cbx_collate.checked
istr_printdlg.l_copies = li_copies
istr_printdlg.b_printtofile = cbx_printtofile.checked
istr_printdlg.l_frompage = ll_frompage
istr_printdlg.l_topage = ll_topage

// Update the b_disablepagenums element which will be
// used to indicate that the default action was taken
istr_printdlg.b_disablepagenums = true

CloseWithReturn (this, istr_printdlg)
end event

event close;call super::close;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  close
//
//	Description:
//	Treat window close from control menu as cancel operation
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	5.0.02   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright $$HEX2$$a9002000$$ENDHEX$$1996-1997 Sybase, Inc. and its subsidiaries.  All rights reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

// If the return code matches the default value,
// then window is currently being closed as a Cancel operation.
if IsNull (istr_printdlg.b_disablepagenums) then
	event pfc_cancel()
end if
end event

event pfc_cancel;call super::pfc_cancel;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  pfc_cancel
//
//	Arguments:  none
//
//	Returns:  none
//
//	Description:
//	Processing when user cancels from dialog
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	5.0.02   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright $$HEX2$$a9002000$$ENDHEX$$1996-1997 Sybase, Inc. and its subsidiaries.  All rights reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

// Update the b_disablepagenums element which will be
// used to indicate that no action was taken (user cancelled)
istr_printdlg.b_disablepagenums = false

CloseWithReturn (this, istr_printdlg)
end event

type cbx_printtofile from u_cbx within pfc_w_print
integer x = 59
integer y = 28
integer width = 1111
integer height = 72
integer taborder = 10
string text = "Print to fi&le"
end type

type rb_all from u_rb within pfc_w_print
integer x = 119
integer y = 216
integer width = 1042
integer height = 72
integer taborder = 20
string text = "&All"
boolean checked = true
end type

type rb_pages from u_rb within pfc_w_print
integer x = 119
integer y = 320
integer width = 512
integer height = 72
boolean bringtotop = true
string text = "Pa&ges"
end type

event clicked;call super::clicked;// Set focus to "from page" when clicked
em_frompage.SetFocus()
end event

type st_1 from u_st within pfc_w_print
integer x = 686
integer y = 316
integer width = 187
integer height = 84
long backcolor = 79741120
string text = "&from:"
alignment alignment = center!
end type

type em_frompage from u_em within pfc_w_print
event keydown pbm_keydown
integer x = 882
integer y = 316
integer width = 151
integer height = 84
integer taborder = 30
string text = "1"
integer accelerator = 102
string mask = "#####"
string displaydata = ""
double increment = 0
string minmax = ""
end type

event keydown;call super::keydown;// Check 'page numbers' when modifying value
if not rb_pages.checked then
	rb_pages.checked = true
end if
end event

event constructor;call super::constructor;ib_autoselect = true
end event

type st_2 from u_st within pfc_w_print
integer x = 1038
integer y = 316
integer width = 187
integer height = 84
string text = "&to:"
alignment alignment = center!
end type

type em_topage from u_em within pfc_w_print
event keydown pbm_keydown
integer x = 1239
integer y = 316
integer width = 151
integer height = 84
integer taborder = 40
boolean bringtotop = true
integer accelerator = 116
string mask = "#####"
string displaydata = ""
double increment = 0
string minmax = ""
end type

event keydown;call super::keydown;// Check 'page numbers' when modifying value
if not rb_pages.checked then
	rb_pages.checked = true
end if
end event

event constructor;call super::constructor;ib_autoselect = true
end event

type rb_selection from u_rb within pfc_w_print
integer x = 119
integer y = 424
integer width = 1051
integer height = 72
boolean bringtotop = true
boolean enabled = false
string text = "&Selection"
end type

type p_collate from u_p within pfc_w_print
integer x = 137
integer y = 780
integer width = 448
integer height = 120
boolean originalsize = true
string picturename = "uncollat.bmp"
end type

type gb_1 from u_gb within pfc_w_print
integer x = 59
integer y = 124
integer width = 1431
integer height = 440
integer taborder = 0
string text = "Print range"
end type

type st_3 from u_st within pfc_w_print
integer x = 69
integer y = 652
integer width = 704
integer height = 72
string text = "Number of &copies:"
alignment alignment = right!
end type

type em_copies from u_em within pfc_w_print
integer x = 805
integer y = 644
integer width = 206
integer height = 84
integer taborder = 50
string text = "1"
integer accelerator = 99
string mask = "####"
boolean spin = true
string displaydata = ""
string minmax = "1~~9999"
end type

event constructor;call super::constructor;ib_autoselect = true
end event

type cbx_collate from u_cbx within pfc_w_print
integer x = 626
integer y = 808
integer width = 809
integer height = 68
integer taborder = 60
string text = "C&ollate"
end type

event clicked;call super::clicked;// Change collate picture based on current collate setting
if this.checked then
	p_collate.picturename = "collat.bmp"
else
	p_collate.picturename = "uncollat.bmp"
end if
end event

type cb_ok from u_cb within pfc_w_print
integer x = 1541
integer y = 144
integer width = 375
integer taborder = 70
string text = "OK"
boolean default = true
end type

event clicked;call super::clicked;parent.event pfc_default()
end event

type cb_cancel from u_cb within pfc_w_print
integer x = 1541
integer y = 268
integer width = 375
integer taborder = 80
boolean bringtotop = true
string text = "Cancel"
boolean cancel = true
end type

event clicked;call super::clicked;parent.event pfc_cancel()
end event

type cb_printer from u_cb within pfc_w_print
integer x = 1541
integer y = 392
integer width = 375
integer taborder = 90
boolean bringtotop = true
string text = "&Printer..."
end type

event clicked;call super::clicked;parent.event pfc_printer()
end event

type gb_2 from u_gb within pfc_w_print
integer x = 59
integer y = 576
integer width = 1431
integer height = 440
integer taborder = 0
string text = "Copies"
end type

