"** 01-s1.pdf page 41
" s7

pibreak:		" priority interrupt break processing "chain"
   dac .ac		" save interrupt AC
	"** CROSSED OUT....
   dpsf			" skip on data phone flag set
   jmp 1f		"  not set: check next device

   dpcf			" clear dataphone flag
   dprs			" read dataphone status
   dac dpstat		" save for user
   sma ral		" REC FLG set? rotate flags left
   jmp 2f		"  no
   dprc			" read char (bit zero set if parity correct)
   dac dpchar		" save for user
   -1
   dac dpread		" dpread = -1
   lac dpstat		" get status back
   ral			" rotate up
2:
   sma			" XMIT FLG set?
   jmp piret		"  no: return from interrupt
   -1
   dac dpwrite		" dpwrite = -1
   jmp piret	"** END OF CROSSOUT

1: clsf			" clock overflow (line frequency ticks)?
   jmp 1f		"  no

   lpb			" load display push buttons
   dac pbsflgs		" save
   isz s.tim+1		" increment low order tick count
   skp			"  no overflow, skip second increment
   isz s.tim		"   low order overflowed, increment high order count
   isz uquant		"    increment user quantum counter
	"** written: ttydelay -> ttyd1
	"** written: ttyrestart -> ttyres1

cnop:			" fetched as constant in iread
   nop			
   -1
   dac 7		" set location 7 to -1 (nothing ever checks/clears it?)
   clon			" enable clock interrupts, reset flag
   lac ttydelay		" tty delay positive?
   spa			"  yes: skip to skp
   isz ttydelay		"   no: done delaying?
   skp			"    not done
   jms ttyrestart	"     yes: start output
	"** START CROSSED OUT: written: lac tty
   lac .dspb
   sna			" .dspb != 0?
   jmp piret		"  no: return
   isz .dsptm		" increment .dsptm; is zero?
   skp			"  no
   jmp dsprestart	" yes: restart display
   sad d3		" .dspb == 3?
   jmp piret		"  yes: return
   isz .dspb		" increment .dspb; is zero?
   jmp piret		"  no: return from interrupt
   jmp dsprestart	" yes: restart display
		"** END CROSSED OUT

1: dssf			" disk flag set?
   jmp 1f		"  no

   -1			" set .dskb = -1
   dac .dskb

"** 01-s1.pdf page 42

   dsrs			" get disk status in .dske
   dac .dske
   dscs			" clear status register
   jmp piret

		"** BEGIN CROSSED OUT
1: lds			" load display status (see 03-scope.pdf pg 25)
   sma ral		" display trap set? (and rotate left)
   jmp 1f		"  not set
   cdf			" display done executing; clear display flags
   lac .dspb
   sna
   jmp piret		" return now if .dspb == 0
   tad dm3
   sna
   jmp dsprestart	" start display if .dspb == 3
   dac .dspb		" otherwise, .dspb -= 3 and return
   jmp piret
dsprestart:
   lac d1
   dac .dspb		" set .dspb = 1
   lac dspbufp		" load display buf pointer
   beg			" start display processor
   -10
   dac .dsptm		" set .dsptm = -10 (10 ticks)
   jmp piret

1: sma ral		" edges flag set?? (and rotate)
   jmp .+3		"  no
   raef			" "resume after edges flag"
   jmp piret		" return
   sma			" light pen flag?
   jmp 1f		"  no
   lda			" G-2: load display address
   dac .lpba		" save
   rlpd			" G-2: resume after light pen stop
   jmp piret

1: ksf			" (TTY) keyboard flag set?
   jmp 1f		"  no

   lac ttydelay		" get TTY delay
   sma			" minus (waiting for output)?
   isz ttydelay		"  no: increment??? (make more positive)
   krb			" read keyboard buffer
   dac char		" save in char
   sad o375		" interrupt char (TTY ALT MODE?)
   jmp intrp1		"  yes
   lac d1
   jms putchar
      dzm char
   lac sfiles+0		" get sleep word for ttyin
   jms wakeup		" wake processes
   dac sfiles+0		" clear sleep word
   lac char		" get character
   sad o212		" new line (with parity)??
   skp			"  yes
   jmp piret		"   no: done
   lac sfiles+1		" get ttyout sleep word
   sma			" highest bit set?
   xor o400000		" no, make it so (why???)
   dac sfiles+1		" save back

"** 01-s1.pdf page 43

   jms putcr		" output CR next
   jms ttyrestart	" start output
   jmp piret

1: tsf			" TTY output flag set?
   jmp 1f		"  no

   tcf			" yes: clear flag
   jms ttyrestart	" transmit next character
   jmp piret

ttyrestart: 0
   lac ttydelay		" get tty delay
   spa			" positive?
   jmp ttyrestart i	"  no: keep waiting
   lac nttychar		" get pending CR, if any
   dzm nttychar		" clear it
   sza			" need to send CR?
   jmp 3f		"  yes
   isz ttydelay		" increment ttydelay (make more positive)
   lac d2
   jms getchar		" get a character
      jmp 2f		"  none found??
3:
   tls			" start output
   sad o12		" newline?
   jms putcr		" yes: put CR next
   sad o15		" CR?
   skp			"  yes
   jmp ttyrestart i	"   no: return
   lac ttydelay		" get current tty delay
   tad o20		" bump by 16
   rcr			" divide by two
   cma			" complement
   dac ttydelay		" save
   jmp ttyrestart i
2:
   lac sfiles+1		" run out of characters to send: wake user(s)
   jms wakeup
   dac sfiles+1
   jmp ttyrestart i	"** written arrow up 2 copies

			"** BEGIN CROSSED OUT
1: sck			" Graphic-2 keyboard flag set?
   jmp 1f		"  no.

   cck			" yes: clear flag
   lck			" read character
   dac char
   sad o33		" code 33 (ESCAPE?)
   jmp intrp2		"  yes: mark interrupt
   lac d3
   jms putchar
      nop
   lac sfiles+2
   jms wakeup
   dac sfiles+2
   jmp piret

1: rsf			" paper tape reader ready?
   jmp 1f		"  no


"** 01-s1.pdf page 44

   lac npptchar
   sna			" have saved char?
   jmp .+5		"  no: jump to second rrb
   dac char		" yes: save as current char
   rrb			" clear flag, read reader buffer
   dac npptchar		" save as saved char
   jmp .+3
   rrb			" here without saved char: read new
   dac char		" save as current
3:
   lac char
   sna
   jmp 2f
   lac d4
   jms putchar
      jmp 3f
   lac char
   sad d4
   jmp 4f
2:
   lac npptchar		" get saved char (if any)
   sna			" had saved char?
   jmp .+4		"  no: wake up writer
   dac char		" yes: save as char to send
   dzm npptchar		" clear saved char
   jmp 3b
   rsa			" reader select alphanumeric mode
   lac sfiles+3
   jms wakeup		" wake sleepers; returns zero
   xor o400000		" set high bit (rsa before sleep)
   dac sfiles+3
   jmp piret
3:
   lac char
   dac npptchar
4:
   lac sfiles+3
   jms wakeup
   dac sfiles+3
   jmp piret

1: psf			" paper tape punch ready?
   jmp 1f		"  no

   pcf			" clear ptp flag
   lac d5
   jms getchar		" get next char
   jmp .+3		"  none: wake sleepers
   psa			" punch set alphanumeric mode
   jmp piret		" return from interrupt
   lac sfiles+4		" get sleeper bit vector
   jms wakeup		" wake them
   dac sfiles+4		" store zero
   jmp piret		" return from PI

		"** BEGIN CROSSED OUT
1: spb			" any graphic-2 push button?
   jmp 1f		"  no

   cpb			" clear push button flag
   lpb			" load push button value
   dac pbsflgs+1

"** 01-s1.pdf page 45

   and o2000		" get push button 7
   sna			" set?
   jmp piret		"  no: done
   jms dspinit		" yes: reset display buffer
   lac sfiles+6		" wake up anyone sleeping on display
   jms wakeup
   dac sfiles+6
   cla			" clear button lights
   wbl
   jmp piret	"** END CROSSED OUT

1: crsf			" card reader flag set?
   jmp 1f		"  no

   crrb
   dac crchar
   -1
   dac crread
   jmp piret

1: crrb			" read card reader buffer??

piret:			" return from priority interrupt
   lac 0		" get LINK (in bit 0)
   ral			" restore LINK
   lac .ac		" restore AC
   ion			" reenable interrupts
   jmp 0 i		" return from interrupt

	" wake sleeping processes
	" NOTE!! Called from interrupt service, so avoids indirect!!!
	" call:
	" AC/ sfiles word (bit vector of processes to wake)
	"   jms wakeup
	" AC/ 0 (to store in sfiles word)
wakeup: 0
   dac 9f+t		" save vector in t0
   -mnproc
   dac 9f+t+1		" loop count in t1
   lac tadu		" get "tad ulist"
   dac 2f
   lac dacu		" get "dac ulist"
   dac 2f+1
1:
   lac 9f+t
   ral			" rotate vector up one
   dac 9f+t
   sma			" high bit set?
   jmp 2f+2		"  no: skip the fun
   lac o700000		" yes: decrement process status (wake)
2: tad ..		" (avoiding indirect)
   dac ..
   lac 2b		" advance tad operand by 4 words
   tad d4
   dac 2b
   lac 2b+1		" advance tad operand by 4 words
   tad d4
   dac 2b+1
   isz 9f+t+1		" done?
   jmp 1b		"  no, keep going
   cla			" return zero in AC
   jmp wakeup i
t = t+2

	" call to output CR after LF (NL) on TTY
putcr: 0
   lac o15
   dac nttychar

"** 01-s1.pdf page 46

   cla
   jmp putcr i

intrp1:			" here with TTY interrupt character
   lac d6		" get keyboard special device number
   dac .int1		" save as interrupt source
   lac d1		" drain tty input buffer?
   jms getchar
      skp
   jmp .-3
   lac d2		" drain tty output buffer?
   jms getchar
      skp
   jmp .-3
   lac sfiles+0		" wake ttyin sleepers
   jms wakeup
   dac sfiles+0
   lac sfiles+1		" wake ttyout sleepers
   jms wakeup
   dac sfiles+1
   jms chkint		" check if user interrupted
      jmp piret		"  no, return from PI
   jmp 1f		" yes: return thru system call code (dump core)
intrp2:			" here with display interrupt character
   lac d7		" get keyboard special device number
   dac .int2		" save as interrupt source
   lac d3		" drain keyboard buffer?
   jms getchar
      skp
   jmp .-3
   lac sfiles+2		" wake up any "keyboard" sleepers
   jms wakeup
   dac sfiles+2
   lac sfiles+6		" wake up any "display" sleepers
   jms wakeup
   dac sfiles+6
   jms chkint		" check if user interrupted
      jmp piret		"  no, return from PI
1:
   lac 0		" get interrupt PC
   dac 020		" save as system call return PC
   lac .ac		" restore AC from interrupt
   jmp 021		" join system call processing (dump core?)
