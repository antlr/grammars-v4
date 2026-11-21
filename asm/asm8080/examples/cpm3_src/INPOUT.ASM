$title	('INP:/OUT: Interface')
	name inpout
	cseg
;
;	CP/M 3 PIP Utility INP: / OUT: Interface module
;	Code org'd at 080h
;	July 5, 1982

public	inploc,outloc,inpd,outd

	org	00h
inpd:
    	call inploc
    	ret

outd:
    	call outloc
    	ret

inploc:
    	mvi a,01Ah
    	ret

outloc:
    	ret
    	nop
    	nop

    	org	07fh
    	db	0
end
EOF
