			"Generic MAIN file for
			    The ZORK Trilogy
		       started on 7/28/83 by MARC"

<CONSTANT SERIAL 0>

<GLOBAL PLAYER <>>

<GLOBAL P-WON <>>

<CONSTANT M-FATAL 2>

<CONSTANT M-HANDLED 1>

<CONSTANT M-NOT-HANDLED <>>

<CONSTANT M-OBJECT <>>

<CONSTANT M-BEG 1>

<CONSTANT M-END 6>

<CONSTANT M-ENTER 2>

<CONSTANT M-LOOK 3>

<CONSTANT M-FLASH 4>

<CONSTANT M-OBJDESC 5>

;"GO now lives in SPECIAL.ZIL"


<ROUTINE MAIN-LOOP ("AUX" TRASH)
	 <REPEAT ()
		 <SET TRASH <MAIN-LOOP-1>>>>

<ROUTINE MAIN-LOOP-1 ("AUX" ICNT OCNT NUM CNT OBJ TBL V PTBL OBJ1 TMP O I)
     <SET CNT 0>
     <SET OBJ <>>
     <SET PTBL T>
     <COND (<SETG P-WON <PARSER>>
	    <SET ICNT <GET ,P-PRSI ,P-MATCHLEN>>
	    <SET OCNT <GET ,P-PRSO ,P-MATCHLEN>>
	    <COND (<AND ,P-IT-OBJECT <ACCESSIBLE? ,P-IT-OBJECT>>
		   <SET TMP <>>
		   <REPEAT ()
			   <COND (<G? <SET CNT <+ .CNT 1>> .ICNT>
				  <RETURN>)
				 (T
				  <COND (<EQUAL? <GET ,P-PRSI .CNT> ,IT>
					 <PUT ,P-PRSI .CNT ,P-IT-OBJECT>
					 <SET TMP T>
					 <RETURN>)>)>>
		   <COND (<NOT .TMP>
			  <SET CNT 0>
			  <REPEAT ()
			   <COND (<G? <SET CNT <+ .CNT 1>> .OCNT>
				  <RETURN>)
				 (T
				  <COND (<EQUAL? <GET ,P-PRSO .CNT> ,IT>
					 <PUT ,P-PRSO .CNT ,P-IT-OBJECT>
					 <RETURN>)>)>>)>
		   <SET CNT 0>)>
	    <SET NUM
		 <COND (<0? .OCNT> .OCNT)
		       (<G? .OCNT 1>
			<SET TBL ,P-PRSO>
			<COND (<0? .ICNT> <SET OBJ <>>)
			      (T <SET OBJ <GET ,P-PRSI 1>>)>
			.OCNT)
		       (<G? .ICNT 1>
			<SET PTBL <>>
			<SET TBL ,P-PRSI>
			<SET OBJ <GET ,P-PRSO 1>>
			.ICNT)
		       (T 1)>>
	    <COND (<AND <NOT .OBJ> <1? .ICNT>> <SET OBJ <GET ,P-PRSI 1>>)>
	    <COND (<AND <==? ,PRSA ,V?WALK>
			<NOT <ZERO? ,P-WALK-DIR>>>
		   <SET V <PERFORM ,PRSA ,PRSO>>)
		  (<0? .NUM>
		   <COND (<0? <BAND <GETB ,P-SYNTAX ,P-SBITS> ,P-SONUMS>>
			  <SET V <PERFORM ,PRSA>>
			  <SETG PRSO <>>)
			 (<NOT ,LIT>
			  <TELL "It's too dark to see." CR>)
			 (T
			  <TELL "It's not clear what you're referring to." CR>
			  <SET V <>>)>)
		  (T
		   <SETG P-NOT-HERE 0>
		   <SETG P-MULT <>>
		   <COND (<G? .NUM 1> <SETG P-MULT T>)>
		   <SET TMP <>>
		   <REPEAT ()
			   <COND (<G? <SET CNT <+ .CNT 1>> .NUM>
				  <COND (<G? ,P-NOT-HERE 0>
					 <TELL "The ">
					 <COND (<NOT <EQUAL? ,P-NOT-HERE .NUM>>
						<TELL "other ">)>
					 <TELL "object">
					 <COND (<NOT <EQUAL? ,P-NOT-HERE 1>>
						<TELL "s">)>
					 <TELL " that you mentioned ">
					 <COND (<NOT <EQUAL? ,P-NOT-HERE 1>>
						<TELL "are">)
					       (T <TELL "is">)>
					 <TELL "n't here." CR>)
					(<NOT .TMP>
					 <TELL
"There's nothing here you can take." CR>)>
				  <RETURN>)
				 (T
				  <COND (.PTBL <SET OBJ1 <GET ,P-PRSO .CNT>>)
					(T <SET OBJ1 <GET ,P-PRSI .CNT>>)>
				  <SET O <COND (.PTBL .OBJ1) (T .OBJ)>>
				  <SET I <COND (.PTBL .OBJ) (T .OBJ1)>>

;"multiple exceptions"
<COND (<OR <G? .NUM 1>
	   <EQUAL? <GET <GET ,P-ITBL ,P-NC1> 0> ,W?ALL>>
       <SET V <LOC ,WINNER>>
       <COND (<EQUAL? .O ,NOT-HERE-OBJECT>
	      <SETG P-NOT-HERE <+ ,P-NOT-HERE 1>>
	      <AGAIN>)
	     (<AND <VERB? TAKE>
		   .I
		   <EQUAL? <GET <GET ,P-ITBL ,P-NC1> 0> ,W?ALL>
		   <NOT <IN? .O .I>>>
	      <AGAIN>)
	     (<AND <EQUAL? ,P-GETFLAGS ,P-ALL>
		   <VERB? TAKE>
		   <OR <AND <NOT <EQUAL? <LOC .O> ,WINNER ,HERE .V>>
			    <NOT <EQUAL? <LOC .O> .I>>
			    <NOT <FSET? <LOC .O> ,SURFACEBIT>>>
		       <NOT <OR <FSET? .O ,TAKEBIT>
				<FSET? .O ,TRYTAKEBIT>>>>>
	      <AGAIN>)
	     (ELSE
	      <COND (<EQUAL? .OBJ1 ,IT>
		     <PRINTD ,P-IT-OBJECT>)
		    (T <PRINTD .OBJ1>)>
	      <TELL ": ">)>)>
;"end multiple exceptions"
				  <SETG PRSO .O>
				  <SETG PRSI .I>
				  <SET TMP T>
				  <SET V <PERFORM ,PRSA ,PRSO ,PRSI>>
				  <COND (<==? .V ,M-FATAL> <RETURN>)>)>>)>
	    <COND (<NOT <==? .V ,M-FATAL>>
		   ;<COND (<==? <LOC ,WINNER> ,PRSO>
			  <SETG PRSO <>>)>
		   <SET V <APPLY <GETP <LOC ,WINNER> ,P?ACTION> ,M-END>>)>
	    ;<COND (<VERB? ;AGAIN ;"WALK -- why was this here?"
			  SAVE RESTORE ;SCORE ;VERSION ;WAIT> T)
		  (T
		   <SETG L-PRSA ,PRSA>
		   <SETG L-PRSO ,PRSO>
		   <SETG L-PRSI ,PRSI>)>
	    <COND (<==? .V ,M-FATAL> <SETG P-CONT <>>)>)
	   (T
	    <SETG P-CONT <>>)>
     %<COND (<==? ,ZORK-NUMBER 3>
	     '<COND (<NOT ,CLEFT-QUEUED?>
		     <ENABLE <QUEUE I-CLEFT <+ 70 <RANDOM 70>>>>
		     <SETG CLEFT-QUEUED? T>)>)
	    (ELSE '<NULL-F>)>
     <COND (,P-WON
	    <COND (<VERB? TELL BRIEF SUPER-BRIEF VERBOSE SAVE VERSION
			  QUIT RESTART SCORE SCRIPT UNSCRIPT RESTORE> T)
		  (T <SET V <CLOCKER>>)>)>>

<GLOBAL P-MULT <>>

<GLOBAL P-NOT-HERE 0>



%<COND (<GASSIGNED? PREDGEN>

'<ROUTINE PERFORM (A "OPTIONAL" (O <>) (I <>) "AUX" V OA OO OI)
	;<COND (,DEBUG
	       <TELL "[Perform: ">
	       %<COND (<GASSIGNED? PREDGEN> '<TELL N .A>)
		      (T '<PRINC <NTH ,ACTIONS <+ <* .A 2> 1>>>)>
	       <COND (<AND .O <NOT <==? .A ,V?WALK>>>
		      <TELL "/" D .O>)>
	       <COND (.I <TELL "/" D .I>)>
	       <TELL "]" CR>)>
	<SET OA ,PRSA>
	<SET OO ,PRSO>
	<SET OI ,PRSI>
	<COND (<AND <EQUAL? ,IT .I .O>
		    <NOT <ACCESSIBLE? ,P-IT-OBJECT>>>
	       <TELL "I don't see what you are referring to." CR>
	       <RFATAL>)>
	<COND (<==? .O ,IT> <SET O ,P-IT-OBJECT>)>
	<COND (<==? .I ,IT> <SET I ,P-IT-OBJECT>)>
	<SETG PRSA .A>
	<SETG PRSO .O>
	<COND (<AND ,PRSO <NOT <EQUAL? ,PRSI ,IT>> <NOT <VERB? WALK>>>
	       <SETG P-IT-OBJECT ,PRSO>)>
	<SETG PRSI .I>
	<COND (<AND <EQUAL? ,NOT-HERE-OBJECT ,PRSO ,PRSI>
		    <SET V <NOT-HERE-OBJECT-F>>> .V)
	      (T
	       <SET O ,PRSO>
	       <SET I ,PRSI>
	       <COND
		(<SET V <APPLY <GETP ,WINNER ,P?ACTION>>> .V)
		(<SET V <APPLY <GETP <LOC ,WINNER> ,P?ACTION> ,M-BEG>> .V)
		(<SET V <APPLY <GET ,PREACTIONS .A>>> .V)
		(<AND .I <SET V <APPLY <GETP .I ,P?ACTION>>>> .V)
		(<AND .O
		      <NOT <==? .A ,V?WALK>>
		      <LOC .O>
		      <SET V <APPLY <GETP <LOC .O> ,P?CONTFCN>>>>
		 .V)
		(<AND .O
		      <NOT <==? .A ,V?WALK>>
		      <SET V <APPLY <GETP .O ,P?ACTION>>>>
		 .V)
		(<SET V <APPLY <GET ,ACTIONS .A>>> .V)>)>
	<SETG PRSA .OA>
	<SETG PRSO .OO>
	<SETG PRSI .OI>
	.V>)
       (T

'<PROG ()

<SETG DEBUG <>>

<ROUTINE PERFORM (A "OPTIONAL" (O <>) (I <>) "AUX" V OA OO OI)
	#DECL ((A) FIX (O) <OR FALSE OBJECT FIX> (I) <OR FALSE OBJECT> (V) ANY)
	<COND (,DEBUG
	       <TELL "** PERFORM: PRSA = ">
	       <PRINC <NTH ,ACTIONS <+ <* .A 2> 1>>>
	       <COND (<AND .O <NOT <==? .A ,V?WALK>>>
		      <TELL " | PRSO = " D .O>)>
	       <COND (.I <TELL " | PRSI = " D .I>)>)>
	<SET OA ,PRSA>
	<SET OO ,PRSO>
	<SET OI ,PRSI>
	<COND (<AND <EQUAL? ,IT .I .O>
		    <NOT <ACCESSIBLE? ,P-IT-OBJECT>>>
	       <TELL "I don't see what you are referring to." CR>
	       <RFATAL>)>
	<COND (<==? .O ,IT> <SET O ,P-IT-OBJECT>)>
	<COND (<==? .I ,IT> <SET I ,P-IT-OBJECT>)>
	<SETG PRSA .A>
	<SETG PRSO .O>
	<COND (<AND ,PRSO <NOT <VERB? WALK>>>
	       <SETG P-IT-OBJECT ,PRSO>)>
	<SETG PRSI .I>
	<COND (<AND <EQUAL? ,NOT-HERE-OBJECT ,PRSO ,PRSI>
		    <SET V <D-APPLY "Not Here" ,NOT-HERE-OBJECT-F>>> .V)
	      (T
	       <SET O ,PRSO>
	       <SET I ,PRSI>
	       <COND (<SET V <DD-APPLY "Actor" ,WINNER
				      <GETP ,WINNER ,P?ACTION>>> .V)
		     (<SET V <D-APPLY "Room (M-BEG)"
				      <GETP <LOC ,WINNER> ,P?ACTION>
				      ,M-BEG>> .V)
		     (<SET V <D-APPLY "Preaction"
				      <GET ,PREACTIONS .A>>> .V)
		     (<AND .I <SET V <D-APPLY "PRSI"
					      <GETP .I ,P?ACTION>>>> .V)
		     (<AND .O
			   <NOT <==? .A ,V?WALK>>
			   <LOC .O>
			   <GETP <LOC .O> ,P?CONTFCN>
			   <SET V <DD-APPLY "Container" <LOC .O>
					   <GETP <LOC .O> ,P?CONTFCN>>>>
		      .V)
		     (<AND .O
			   <NOT <==? .A ,V?WALK>>
			   <SET V <D-APPLY "PRSO"
					   <GETP .O ,P?ACTION>>>>
		      .V)
		     (<SET V <D-APPLY <>
				      <GET ,ACTIONS .A>>> .V)>)>
	<SETG PRSA .OA>
	<SETG PRSO .OO>
	<SETG PRSI .OI>
	.V>

<DEFINE D-APPLY (STR FCN "OPTIONAL" FOO "AUX" RES)
	<COND (<NOT .FCN> <>)
	      (T
	       <COND (,DEBUG
		      <COND (<NOT .STR>
			     <TELL CR "  Default ->" CR>)
			    (T <TELL CR "  " .STR " -> ">)>)>
	       <SET RES
		    <COND (<ASSIGNED? FOO>
			   <APPLY .FCN .FOO>)
			  (T <APPLY .FCN>)>>
	       <COND (<AND ,DEBUG .STR>
		      <COND (<==? .RES 2>
			     <TELL "Fatal" CR>)
			    (<NOT .RES>
			     <TELL "Not handled">)
			    (T <TELL "Handled" CR>)>)>
	       .RES)>>

<ROUTINE DD-APPLY (STR OBJ FCN "OPTIONAL" (FOO <>))
	<COND (,DEBUG <TELL "[" D .OBJ "=]">)>
	<D-APPLY .STR .FCN .FOO>>
>)>
