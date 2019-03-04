" Warren's date program.

   sys time		" Get time in sixtieths since beginning of year
   cll
   div; 216000		" Divide by number of sixtieths in an hour.
			" At this point MQ=number of hours since the
			" beginning of the year, and AC is the number
			" of sixtieths since the last o'clock.
   dac sixtieths	" Save AC and work on the hours & days
   lacq			" Move the number of hours into the AC
   idiv; 24
   dac hours		" Save the remainder as the number of hours 
   lacq			" The quotient is the number of days so far this year
   dac dayinyear
			
1: lmq			" Copy current number of days into MQ before subtract
   tad daysinmonth i	" Subtract days in the month
   sma sza
     jmp 2f		" Result was >0, not this month
   jmp 3f
2: isz curmonth		" Not this month, so move up
   isz daysinmonth	" and try the next one
   jmp 1b

3: lac dayinyear	" Get the num days so far this year
   idiv; 7		" Modulo 7 to get the day number in the week
   tad dayptr		" Add to the base day name pointer
   dac dayptr
   lac dayptr i		" Save the base of this day name string
   dac 1f
   lac d1
   sys write; 1:0; 2	" and print it out

   lac curmonth i	" Get the pointer to the month name
   dac 1f
   lac d1		" Print out the month name
   sys write; 1:0; 2

   lacq			" Get back the days in this month from MQ
			" which we had at the jmp 3f way back
   tad d1		" and print them out +1
   jms decprnt; -2
   jms seventy		" followed by " 1970 "
   lac hours
   jms decprnt; -2	" Print the number of hours
   jms colon
   lac sixtieths	" Now get the sixtieths back and divide by 60
   idiv; 60		" to lose the sixtieths that we don't care about.
   lacq
   idiv; 60		" Get the remainder as the number of seconds into AC
   dac seconds		" and save it
   lacq			" Get the quotient as the number of minutes
   jms decprnt; -2	" and print it
   jms colon
   lac seconds		" Finally print the seconds out
   jms decprnt; -2
   lac d1
   sys write; newline; 1
   sys exit		" Boy, what an effort!

decprnt: 0		" Routine to print out a number in decimal
   dac num
   lac endptr		" Point at the end of the buffer
   dac dbufptr
   dzm count		" and set no characters so far
   lac num
1: cll
   sza			" Is there anything left in the number?
     jmp 3f
   lac o60		" No, so put a space into the buffer
   jmp 4f

3: idiv; 10		" Divide AC by 10
   tad o60		" Add ASCII '0'
4: dac dbufptr i	" and save the character into the buffer
   -1			" Move pointer back a word
   tad dbufptr
   dac dbufptr
   isz count		" Bump up the count of characters
   lacq			" and move the quotient into AC
   isz decprnt i        " Add 1 to the # digits the user wants
     jmp 1b             " Loop back for the next digit

5: isz dbufptr		" Restore the pointer to the first digit
   lac d1		" Print as a string on stdout
   sys write; dbufptr:dbufend; count:0
   isz decprnt
   jmp decprnt i	" and return from the routine


colon: 0		" Print out a colon
   lac d1
   sys write; colonstr; 1
   jmp colon i
 
seventy: 0		" Print out " 1970 "
   lac d1
   sys write; seventystr; 3
   jmp seventy i
 

" When doing the decimal conversion, we set aside 5 words
" to buffer the characters, and we write from the end
" backwards to the beginning of the buffer
dbuf: .=.+4		" First 4 words in the buffer
dbufend: .=.+1		" and the last word
endptr: dbufend

d1: 1			" File descriptor 1 = stdout
d10: 10			" Divide by 10
o60: 060		" ASCII space
num: 0			" Argument to the decimal routine, temp storage
colonstr: 072		" ASCII colon character
seventystr: 040061; <97>; <0 040
newline: 012		" ASCII newline

sixtieths: 0		" Storage for the date and time components
seconds: 0
minutes: 0
hours: 0
dayinyear: 0

" Array of month names pointers
" plus a pointer to the base.
curmonth: montharray
montharray:
   jan; feb; mar; apr
   may; jun; jul; aug
   sep; oct; nov; dec

jan: <Ja>; <n 040
feb: <Fe>; <b 040
mar: <Ma>; <r 040
apr: <Ap>; <r 040
may: <Ma>; <y 040
jun: <Ju>; <n 040
jul: <Ju>; <l 040
aug: <Au>; <g 040
sep: <Se>; <p 040
oct: <Oc>; <t 040
nov: <No>; <v 040
dec: <De>; <c 040

" Array of days in each month
" plus a pointer to the base.
daysinmonth: dayarray
dayarray:
   -31; -29; -31
   -30; -31; -30
   -31; -31; -30
   -31; -30; -31

" Array of day strings
" and pointer to the base
dayptr: daylist
daylist: thu; fri; sat		" Jan 1 1970 is a Thursday
   sun; mon; tue; wed

sun: <Su>; <n 040
mon: <Mo>; <n 040
tue: <Tu>; <e 040
wed: <We>; <d 040
thu: <Th>; <u 040
fri: <Fr>; <i 040
sat: <Sa>; <t 040
