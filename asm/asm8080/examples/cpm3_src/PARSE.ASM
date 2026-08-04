$title	('Filename Parser')
	name	Parse
	public parse
	CSEG
	; BC->.(.filename,.fcb)
	;
	; filename = [d:]file[.type][;password]
	;             
	; fcb assignments
	;
	;   0     => drive, 0 = default, 1 = A, 2 = B, ...
	;   1-8   => file, converted to upper case,
	;            padded with blanks
	;   9-11  => type, converted to upper case,
	;	     padded with blanks
	;   12-15 => set to zero
	;   16-23 => password, converted to upper case,
	;	     padded with blanks
	;   24-25 => address of password field in 'filename',
	;	     set to zero if password length = 0
	;   26    => length of password (0 - 8)
	;
	; Upon return, HL is set to FFFFH if BC locates
	;            an invalid file name;
	; otherwise, HL is set to 0000H if the delimiter
	;            following the file name is a 00H (NULL)
	; 	     or a 0DH (CR);
	; otherwise, HL is set to the address of the delimiter
	;            following the file name.
	;
parse:	lxi h,0
	push h
	push h
	mov h,b
	mov l,c
	mov e,m
	inx h
	mov d,m
	inx h		
	mov a,m
	inx h
	mov h,m
	mov l,a	
	call deblnk
	call delim
	jnz parse1
	mov a,c
	ora a
	jnz parse9
	mov m,a
	jmp parse3
parse1:	mov b,a
	inx d
	ldax d
	cpi ':'
	jnz parse2
	mov a,b
	sui 'A'
	jc parse9
	cpi 16
	jnc parse9
	inr a
	mov m,a
	inx d
	call delim
	jnz parse3
	cpi '.'
	jz parse9
	cpi ':'
	jz parse9
	cpi ';'
	jz parse9
	jmp parse3
parse2:	dcx d
	mvi m,0
parse3:	mvi b,8
	call setfld
	mvi b,3
	cpi '.'
	jz parse4
	call padfld
	jmp parse5
parse4:	inx d
	call setfld
parse5:	mvi b,4
parse6:	inx h
	mvi m,0
	dcr b
	jnz parse6
	mvi b,8
	cpi ';'
	jz parse7
	call padfld
	jmp parse8
parse7:	inx d
	call pwfld
parse8:	push d
	call deblnk
	call delim
	jnz pars81
	inx sp
	inx sp
	jmp pars82
pars81: pop d
pars82: mov a,c
	ora a
	pop b
	mov a,c
	pop b
	inx h
	mov m,c
	inx h
	mov m,b
	inx h
	mov m,a
	xchg
	rnz
	lxi h,0
	ret
parse9:	pop h
	pop h
	lxi h,0ffffh
	ret

setfld:	call delim
	jz padfld
	inx h
	cpi '*'
	jnz setfd1
	mvi m,'?'
	dcr b
	jnz setfld
	jmp setfd2
setfd1: mov m,a
	dcr b
setfd2: inx d
	jnz setfld
setfd3: call delim
	rz
	pop h
	jmp parse9

pwfld:	call delim
	jz padfld
	inx sp
	inx sp
	inx sp
	inx sp
	inx sp
	inx sp
	push d
	push h
	mvi l,0
	xthl
	dcx sp
	dcx sp
pwfld1:	inx sp
	inx sp
	xthl
	inr l
	xthl
	dcx sp
	dcx sp
	inx h
	mov m,a
	inx d
	dcr b
	jz setfd3
	call delim
	jnz pwfld1
	;jmp padfld

padfld:	inx h
	mvi m,' '
	dcr b
	jnz padfld
	ret

delim:	ldax d
	mov c,a
	ora a
	rz
	mvi c,0
	cpi 0dh
	rz
	mov c,a
	cpi 09h
	rz
	cpi ' '
	jc delim2
	rz
	cpi '.'
	rz
	cpi ':'
	rz
	cpi ';'
	rz
	cpi '='
	rz
	cpi ','
	rz
	cpi '/'
	rz
	cpi '['
	rz
	cpi ']'
	rz
	cpi '<'
	rz
	cpi '>'
	rz
	cpi 'a'
	rc
	cpi 'z'+1
	jnc delim1
	ani 05fh
delim1:	ani 07fh
	ret
delim2:	pop h
	jmp parse9

deblnk: ldax d
	cpi ' '
	jz dblnk1
	cpi 09h
	jz dblnk1
	ret
dblnk1: inx d
	jmp deblnk
	END
	EOF


