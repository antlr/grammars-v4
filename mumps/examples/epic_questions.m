; - Developed and tested on Intersystems Cache
; - Scripts are designed to c/p into the Terminal
; with `zr` unloading the buffer and `zs` saving the routine
; - Debug mode was adapted to test the iterative nature of
; problem 3.

; debug mode on
set debug = 1

; xloadmozart
zr
	set ^XA(1)="ONCE UPON A TIME A COMPOSER NAMED MOZART WROTE"
	set ^XA(2)="THE 'MOZART PIANO CONCERTO NUMBER ONE'. MOZART,"
	set ^XA(3)="MOZART, MOMOZART MOZARTMOZART, MOZART."
	set ^XA(4)="THIS! THAT MOZART @# 12323 ` ~ {} /? œ§ MOZART ÀÇË "
	set ^XA(5)="HAPPILY EVER AFTER"
zs xloadmozart

; xproblem1
zr
	Read !,"Enter an Address:",addr
	If addr?.E1" "5N do ;any # of letters followed by " " and 5 numbers
	. w !,"'",addr,"' - has a valid zipcode"
	Else  do
	. w !,"'",addr,"' - has an invalid zipcode"
zs xproblem1
; d ^xproblem1


; xproblem2
; originally I had a simple while loop with $replace function
; but that had underwhelming results due to lack of context and
; the problem's requirement to replace "words" not substrings
; I employed an O(nm) [n string length and m pattern matching] 
; cursor walk to identify words (?.A1P)
; and replace the word "MOZART" /w "BACH" *after* a word was id-ed
zr
	set key = $order(^XA(""))
	while (key '= ""){
		set origword = "MOZART"
		set replword = "BACH"
		set x = ^XA(key)
		set offset = $length(origword) - $length(replword)
		set offsetcnt = 0
		set cursorstr = 1
		set cursorend = 1
		while(cursorend <= $length(x)){
			set cursorspan = $extract(x,cursorstr,cursorend)
			if $get(debug,0)  w !,"Cursor Char:",cursorstr,":",cursorend,":`",cursorspan,"`"
			; if the cursors are together and on puncuation increment both
			if ((cursorspan?1P)&(cursorstr=cursorend)){
				if $get(debug,0) w "->(punc)"
				set cursorstr = cursorstr + 1
				set cursorend = cursorend + 1
			}
			else {
				; if a word is identified replace origword with replword
				if ($extract(x,cursorstr,cursorend)?.A1P){
					set word = $extract(x,cursorstr,cursorend-1)
					if $get(debug,0)  {w "->(word)"}
					if word=origword{
						set cursorstrjst = cursorstr - (offset*offsetcnt)
						set cursorendjst = cursorend - (offset*offsetcnt)
						set left = $extract(^XA(key),1,cursorstrjst-1)
						set mid = replword
						set right = $extract(^XA(key),cursorendjst,$length(^XA(key)))
						if $get(debug,0) w !,left_mid_right
						set ^XA(key) = left_mid_right
						set offsetcnt = offsetcnt + 1
					}
					set cursorstr = cursorend
				}
				else{
					; same internal logic as the (?1P) oh well...
					if ($extract(x,cursorend)'?.AP){
						if $get(debug,0) w "->(Invalid Char -'?AP- `", $extract(x,cursorend),"`)"
						set cursorstr = cursorstr + 1
						set cursorend = cursorend + 1
					}
					else{
						set cursorend = cursorend + 1
					}
				}
			}
		}
		if $get(debug,0) w !,!,"Line-",key,"-BEFORE:",x
		if $get(debug,0) w !,"Line-",key,"- AFTER:",^XA(key),!
		set key = $order(^XA(key))
	}
zs xproblem2
; Load test data and run 
; d ^xloadmozart
; d ^xproblem2


;xproblem3
zr
	kill xcount
	set x = "^XA"
	set x = $query(@x)
	set f=0
	; tried a different iteration structure with $query
	; 
	for  do  quit:f=1
	. if $get(debug,0) write x,":",$length($get(@x)),":",$get(@x),!
	. for j=1:1:$length($get(@x))  do
	.. set char=$extract($get(@x),j)
	.. if ($Ascii(char)?.ANP) set xcount($Ascii(char))=$get(xcount($Ascii(char)),0)+1
	.. if $get(debug,0) if $Ascii(char)'?.ANP w "`",$Ascii(char),"` is not a letter, number, or punctuation"
	. set x = $query(@x)
	. if $length(x)=0 set f=1
	
	set ct = $order(xcount(""))
	while(ct '= ""){
		set charcount = $get(xcount(ct),0)
		if charcount > 0 w $char(ct),":",charcount,!
		set ct = $order(xcount(ct)) 
	}
	w "(All other character count are '0')",!,!
zs xproblem3
; Load test data and run 
; d ^xloadmozart
; d ^xproblem2
; d ^xproblem3


;xproblem4
zr
	kill ^XNAMES
	set x="start"
	for  do  quit:x=""
	. read !,"Enter a name ("" to quit) :",x
	. ; It is unclear from the problem whether a valid name can contain lowercase characters
    . ; if x?.A1",".A1" ".A do
	. if x?.U1",".U1" ".U do
	.. set ^XNAMES(x) = ""
	. else  do
	.. if x="" do
	... quit
	.. else  do
	... w !,"Invalid Name, Format: (LASTNAME,FIRSTNAME INITAL(S)"
	set key = $order(^XNAMES(""))
	; $order is clearly the ideal way for general array traversal
	while (key '= ""){
		write !,key
		set key = $order(^XNAMES(key))
	}
zs xproblem4
; d ^xproblem4


;loadxnames
zr 
	set ^XNAMES("AAA,BBB ER")=""
	set ^XNAMES("BOO,AAA G")=""
	set ^XNAMES("BOO,HUU AR")=""
	set ^XNAMES("BOC,DO R")=""
	set ^XNAMES("AAB,BOO G")=""
zs loadxnames


;xproblem5
zr
	read !,"Find names (starting with):",name
	set key = $order(^XNAMES(""))
	set pattern = "1"""_name_""".E"
	w !,"List of Matching Names",!,"======================"
	while (key '= ""){
		; didn't want to have to do this... but this was the only way
		; I could get pattern matching to work against a variable
		set boolstr = "set bool = (key?"_pattern_")"
		x boolstr
		if (bool){
			w !,key
		}
		set key = $order(^XNAMES(key))
	}
	w !,"======================"
zs xproblem5
; Load run names and then run name find
; d ^loadxnames
; d ^xproblem5
