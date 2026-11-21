"1ACTIONS for
	        Zork I: The Great Underground Empire
	(c) Copyright 1983 Infocom, Inc. All Rights Reserved."

"SUBTITLE THE WHITE HOUSE"

<ROUTINE WEST-HOUSE (RARG)
	 <COND (<EQUAL? .RARG ,M-LOOK>
		<TELL
"You are standing in an open field west of a white house, with a boarded
front door.">
		<COND (,WON-FLAG
		       <TELL
" A secret path leads southwest into the forest.">)>
		<CRLF>)>>

<ROUTINE EAST-HOUSE (RARG)
	 <COND (<EQUAL? .RARG ,M-LOOK>
		<TELL
"You are behind the white house. A path leads into the forest
to the east. In one corner of the house there is a small window
which is ">
		<COND (<FSET? ,KITCHEN-WINDOW ,OPENBIT>
		       <TELL "open.">)
		      (T <TELL "slightly ajar.">)>
		<CRLF>)>>

<ROUTINE OPEN-CLOSE (OBJ STROPN STRCLS)
	 <COND (<VERB? OPEN>
		<COND (<FSET? .OBJ ,OPENBIT>
		       <TELL <PICK-ONE ,DUMMY>>)
		      (T
		       <TELL .STROPN>
		       <FSET .OBJ ,OPENBIT>)>
		<CRLF>)
	       (<VERB? CLOSE>
		<COND (<FSET? .OBJ ,OPENBIT>
		       <TELL .STRCLS>
		       <FCLEAR .OBJ ,OPENBIT>
		       T)
		      (T <TELL <PICK-ONE ,DUMMY>>)>
		<CRLF>)>>

<ROUTINE BOARD-F ()
	 <COND (<VERB? TAKE EXAMINE>
		<TELL "The boards are securely fastened." CR>)>>

<ROUTINE TEETH-F ()
	 <COND (<AND <VERB? BRUSH>
		     <EQUAL? ,PRSO ,TEETH>>
		<COND (<AND <EQUAL? ,PRSI ,PUTTY>
			    <IN? ,PRSI ,WINNER>>
		       <JIGS-UP
"Well, you seem to have been brushing your teeth with some sort of
glue. As a result, your mouth gets glued together (with your nose)
and you die of respiratory failure.">)
		      (<NOT ,PRSI>
		       <TELL
"Dental hygiene is highly recommended, but I'm not sure what you want
to brush them with." CR>)
		      (T
		       <TELL "A nice idea, but with a " D ,PRSI "?" CR>)>)>>

<ROUTINE GRANITE-WALL-F ()
	 <COND (<EQUAL? ,HERE ,NORTH-TEMPLE>
		<COND (<VERB? FIND>
		       <TELL "The west wall is solid granite here." CR>)
		      (<VERB? TAKE RAISE LOWER>
		       <TELL "It's solid granite." CR>)>)
	       (<EQUAL? ,HERE ,TREASURE-ROOM>
		<COND (<VERB? FIND>
		       <TELL "The east wall is solid granite here." CR>)
		      (<VERB? TAKE RAISE LOWER>
		       <TELL "It's solid granite." CR>)>)
	       (<EQUAL? ,HERE ,SLIDE-ROOM>
		<COND (<VERB? FIND READ>
		       <TELL "It only SAYS \"Granite Wall\"." CR>)
		      (T <TELL "The wall isn't granite." CR>)>)
	       (T
		<TELL "There is no granite wall here." CR>)>>

<ROUTINE SONGBIRD-F ()
	 <COND (<VERB? FIND TAKE>
		<TELL "The songbird is not here but is probably nearby." CR>)
	       (<VERB? LISTEN>
		<TELL "You can't hear the songbird now." CR>)
	       (<VERB? FOLLOW>
		<TELL "It can't be followed." CR>)
	       (T
		<TELL "You can't see any songbird here." CR>)>>

<ROUTINE WHITE-HOUSE-F ()
    <COND (<EQUAL? ,HERE ,KITCHEN ,LIVING-ROOM ,ATTIC>
	   <COND (<VERB? FIND>
		  <TELL "Why not find your brains?" CR>)
		 (<VERB? WALK-AROUND>
		  <GO-NEXT ,IN-HOUSE-AROUND>
		  T)>)
	  (<NOT <OR <EQUAL? ,HERE ,EAST-OF-HOUSE ,WEST-OF-HOUSE>
		    <EQUAL? ,HERE ,NORTH-OF-HOUSE ,SOUTH-OF-HOUSE>>>
	   <COND (<VERB? FIND>
		  <COND (<EQUAL? ,HERE ,CLEARING>
			 <TELL "It seems to be to the west." CR>)
			(T
			 <TELL "It was here just a minute ago...." CR>)>)
		 (T <TELL "You're not at the house." CR>)>)
	  (<VERB? FIND>
	   <TELL
"It's right here! Are you blind or something?" CR>)
	  (<VERB? WALK-AROUND>
	   <GO-NEXT ,HOUSE-AROUND>
	   T)
	  (<VERB? EXAMINE>
	   <TELL
"The house is a beautiful colonial house which is painted white.
It is clear that the owners must have been extremely wealthy." CR>)
	  (<VERB? THROUGH OPEN>
	   <COND (<EQUAL? ,HERE ,EAST-OF-HOUSE>
		  <COND (<FSET? ,KITCHEN-WINDOW ,OPENBIT>
			 <GOTO ,KITCHEN>)
			(T
			 <TELL "The window is closed." CR>
			 <THIS-IS-IT ,KITCHEN-WINDOW>)>)
		 (T
		  <TELL "I can't see how to get in from here." CR>)>)
	  (<VERB? BURN>
	   <TELL "You must be joking." CR>)>>

;"0 -> no next, 1 -> success, 2 -> failed move"

<ROUTINE GO-NEXT (TBL "AUX" VAL)
	 <COND (<SET VAL <LKP ,HERE .TBL>>
		<COND (<NOT <GOTO .VAL>> 2)
		      (T 1)>)>>

<ROUTINE FOREST-F ()
	 <COND (<VERB? WALK-AROUND>
		<COND (<OR <EQUAL? ,HERE
			       ,WEST-OF-HOUSE ,NORTH-OF-HOUSE
			       ,SOUTH-OF-HOUSE>
			   <EQUAL? ,HERE ,EAST-OF-HOUSE>>
		       <TELL "You aren't even in the forest." CR>)>
		<GO-NEXT ,FOREST-AROUND>)
	       (<VERB? DISEMBARK>
		<TELL "You will have to specify a direction." CR>)
	       (<VERB? FIND>
		<TELL "You cannot see the forest for the trees." CR>)
	       (<VERB? LISTEN>
		<TELL "The pines and the hemlocks seem to be murmuring."
		      CR>)>>

<ROUTINE MOUNTAIN-RANGE-F ()
	 <COND (<VERB? CLIMB-UP CLIMB-DOWN CLIMB-FOO>
		<TELL "Don't you believe me? The mountains are impassable!"
		      CR>)>>

<ROUTINE WATER-F ("AUX" AV W PI?)
	 <COND (<VERB? SGIVE> <RFALSE>)
	       (<VERB? THROUGH BOARD>
		<TELL <PICK-ONE ,SWIMYUKS> CR>
		<RTRUE>)
	       (<VERB? FILL>	;"fill bottle with water =>"
		<SET W ,PRSI>	   ;"put water in bottle"
		<SETG PRSA ,V?PUT>
		<SETG PRSI ,PRSO>
		<SETG PRSO .W>
		<SET PI? <>>)
	       (<OR <EQUAL? ,PRSO ,GLOBAL-WATER>
		    <EQUAL? ,PRSO ,WATER>>
		<SET W ,PRSO>
		<SET PI? <>>)
	       (ELSE
		<SET W ,PRSI>
		<COND (.W <SET PI? T>)>)>
	 <COND (<EQUAL? .W ,GLOBAL-WATER>
		<SET W ,WATER>
		<COND (<VERB? TAKE PUT> <REMOVE-CAREFULLY .W>)>)>
	 <COND (.PI? <SETG PRSI .W>)
	       (T <SETG PRSO .W>)>
	 <SET AV <LOC ,WINNER>>
	 <COND (<NOT <FSET? .AV ,VEHBIT>> <SET AV <>>)>
	 <COND (<AND <VERB? TAKE PUT> <NOT .PI?>>
		<COND (<AND .AV
			    <OR <EQUAL? .AV ,PRSI>
				<AND <NOT ,PRSI>
				     <NOT <IN? .W .AV>>>>>
		       <TELL "There is now a puddle in the bottom of the "
			     D .AV "." CR>
		       <REMOVE-CAREFULLY ,PRSO>
		       <MOVE ,PRSO .AV>)
		      (<AND ,PRSI <NOT <EQUAL? ,PRSI ,BOTTLE>>>
		       <TELL "The water leaks out of the " D ,PRSI
			     " and evaporates immediately." CR>
		       <REMOVE-CAREFULLY .W>)
		      (<IN? ,BOTTLE ,WINNER>
		       <COND (<NOT <FSET? ,BOTTLE ,OPENBIT>>
			      <TELL "The bottle is closed." CR>
			      <THIS-IS-IT ,BOTTLE>)
			     (<NOT <FIRST? ,BOTTLE>>
			      <MOVE ,WATER ,BOTTLE>
			      <TELL "The bottle is now full of water." CR>)
			     (T
			      <TELL "The water slips through your fingers." CR>
			      <RTRUE>)>)
		      (<AND <IN? ,PRSO ,BOTTLE>
			    <VERB? TAKE>
			    <NOT ,PRSI>>
		       <TELL
"It's in the bottle. Perhaps you should take that instead." CR>)
		      (T
		       <TELL "The water slips through your fingers." CR>)>)
	       (.PI?
		<COND (<AND <VERB? PUT>
			    <GLOBAL-IN? ,RIVER ,HERE>>
		       <PERFORM ,V?PUT ,PRSO ,RIVER>)
		      (ELSE
		       <TELL "Nice try." CR>)>
		<RTRUE>)
	       (<VERB? DROP GIVE>
		<COND (<AND <VERB? DROP>
			    <IN? ,WATER ,BOTTLE>
			    <NOT <FSET? ,BOTTLE ,OPENBIT>>>
		       <TELL "The bottle is closed." CR>
		       <RTRUE>)>
		<REMOVE-CAREFULLY ,WATER>
		<COND (.AV
		       <TELL "There is now a puddle in the bottom of the "
			     D .AV "." CR>
		       <MOVE ,WATER .AV>)
		      (T
		       <TELL
"The water spills to the floor and evaporates immediately." CR>
		       <REMOVE-CAREFULLY ,WATER>)>)
	       (<VERB? THROW>
		<TELL
"The water splashes on the walls and evaporates immediately." CR>
		<REMOVE-CAREFULLY ,WATER>)>>

<GLOBAL KITCHEN-WINDOW-FLAG <>>

<ROUTINE KITCHEN-WINDOW-F ()
	 <COND (<VERB? OPEN CLOSE>
		<SETG KITCHEN-WINDOW-FLAG T>
		<OPEN-CLOSE ,KITCHEN-WINDOW
"With great effort, you open the window far enough to allow entry."
"The window closes (more easily than it opened).">)
	       (<AND <VERB? EXAMINE>
		     <NOT ,KITCHEN-WINDOW-FLAG>>
		<TELL
"The window is slightly ajar, but not enough to allow entry." CR>)
	       (<VERB? WALK BOARD THROUGH>
		<COND (<EQUAL? ,HERE ,KITCHEN>
		       <DO-WALK ,P?EAST>)
		      (T
		       <DO-WALK ,P?WEST>)>
		<RTRUE>)
	       (<VERB? LOOK-INSIDE>
		<TELL "You can see ">
		<COND (<EQUAL? ,HERE ,KITCHEN>
		       <TELL "a clear area leading towards a forest." CR>)
		      (T
		       <TELL "what appears to be a kitchen." CR>)>)>>

<ROUTINE GHOSTS-F ()
	 <COND (<VERB? TELL>
		<TELL "The spirits jeer loudly and ignore you." CR>
		<SETG P-CONT <>>)
	       (<VERB? EXORCISE>
		<TELL "Only the ceremony itself has any effect." CR>)
	       (<AND <VERB? ATTACK MUNG> <EQUAL? ,PRSO ,GHOSTS>>
		<TELL "How can you attack a spirit with material objects?" CR>)
	       (T
		<TELL "You seem unable to interact with these spirits." CR>)>>

<GLOBAL CAGE-TOP T>

<ROUTINE BASKET-F ()
	 <COND (<VERB? RAISE>
		<COND (,CAGE-TOP
		       <TELL <PICK-ONE ,DUMMY> CR>)
		      (T
		       <MOVE ,RAISED-BASKET ,SHAFT-ROOM>
		       <MOVE ,LOWERED-BASKET ,LOWER-SHAFT>
		       <SETG CAGE-TOP T>
		       <THIS-IS-IT ,RAISED-BASKET>
		       <TELL
"The basket is raised to the top of the shaft." CR>)>)
	       (<VERB? LOWER>
		<COND (<NOT ,CAGE-TOP>
		       <TELL <PICK-ONE ,DUMMY> CR>)
		      (T
		       <MOVE ,RAISED-BASKET ,LOWER-SHAFT>
		       <MOVE ,LOWERED-BASKET ,SHAFT-ROOM>
		       <THIS-IS-IT ,LOWERED-BASKET>
		       <TELL
"The basket is lowered to the bottom of the shaft." CR>
		       <SETG CAGE-TOP <>>
		       <COND (<AND ,LIT <NOT <SETG LIT <LIT? ,HERE>>>>
			      <TELL "It is now pitch black." CR>)>
		       T)>)
	       (<OR <EQUAL? ,PRSO ,LOWERED-BASKET>
		    <EQUAL? ,PRSI ,LOWERED-BASKET>>
		<TELL "The basket is at the other end of the chain." CR>)
	       (<AND <VERB? TAKE>
		     <EQUAL? ,PRSO ,RAISED-BASKET ,LOWERED-BASKET>>
		<TELL "The cage is securely fastened to the iron chain." CR>)>>

<ROUTINE BAT-F ()
	 <COND (<VERB? TELL>
		<FWEEP 6>
		<SETG P-CONT <>>)
	       (<VERB? TAKE ATTACK MUNG>
		<COND (<EQUAL? <LOC ,GARLIC> ,WINNER ,HERE>
		       <TELL "You can't reach him; he's on the ceiling." CR>)
		      (T <FLY-ME>)>)>>

<ROUTINE FLY-ME ()
	 <FWEEP 4>
	 <TELL
"The bat grabs you by the scruff of your neck and lifts you away...." CR CR>
	 <GOTO <PICK-ONE ,BAT-DROPS> <>>
	 <COND (<NOT <EQUAL? ,HERE ,ENTRANCE-TO-HADES>>
		<V-FIRST-LOOK>)>
	 T>

<ROUTINE FWEEP (N)
	 <REPEAT ()
		 <COND (<L? <SET N <- .N 1>> 1> <RETURN>)
		       (T <TELL "    Fweep!" CR>)>>
	 <CRLF>>

<GLOBAL BAT-DROPS
      <LTABLE 0
	      MINE-1
	      MINE-2
	      MINE-3
	      MINE-4
	      LADDER-TOP
	      LADDER-BOTTOM
	      SQUEEKY-ROOM
	      MINE-ENTRANCE>>

<ROUTINE BELL-F ()
	 <COND (<VERB? RING>
		<COND (<AND <EQUAL? ,HERE ,LLD-ROOM>
			    <NOT ,LLD-FLAG>>
		       <RFALSE>)
		      (T
		       <TELL "Ding, dong." CR>)>)>>

<ROUTINE HOT-BELL-F ()
	 <COND (<VERB? TAKE>
		<TELL "The bell is very hot and cannot be taken." CR>)
	       (<OR <VERB? RUB> <AND <VERB? RING> ,PRSI>>
		<COND (<FSET? ,PRSI ,BURNBIT>
		       <TELL "The " D ,PRSI " burns and is consumed." CR>
		       <REMOVE-CAREFULLY ,PRSI>)
		      (<EQUAL? ,PRSI ,HANDS>
		       <TELL "The bell is too hot to touch." CR>)
		      (T
		       <TELL "The heat from the bell is too intense." CR>)>)
	       (<VERB? POUR-ON>
		<REMOVE-CAREFULLY ,PRSO>
		<TELL "The water cools the bell and is evaporated." CR>
		<QUEUE I-XBH 0>
		<I-XBH>)
	       (<VERB? RING>
		<TELL "The bell is too hot to reach." CR>)>>

<ROUTINE BOARDED-WINDOW-FCN ()
	 <COND (<VERB? OPEN>
		<TELL "The windows are boarded and can't be opened." CR>)
	       (<VERB? MUNG>
		<TELL "You can't break the windows open." CR>)>>

<ROUTINE NAILS-PSEUDO ()
	 <COND (<VERB? TAKE>
		<TELL
"The nails, deeply imbedded in the door, cannot be removed." CR>)>>

<ROUTINE CRACK-FCN ()
	 <COND (<VERB? THROUGH>
		<TELL "You can't fit through the crack." CR>)>>

<ROUTINE KITCHEN-FCN (RARG)
	<COND (<EQUAL? .RARG ,M-LOOK>
	       <TELL
"You are in the kitchen of the white house. A table seems to
have been used recently for the preparation of food. A passage
leads to the west and a dark staircase can be seen leading
upward. A dark chimney leads down and to the east is a small
window which is ">
	       <COND (<FSET? ,KITCHEN-WINDOW ,OPENBIT>
		      <TELL "open." CR>)
		     (T
		      <TELL "slightly ajar." CR>)>)
	      (<==? .RARG ,M-BEG>
	       <COND (<AND <VERB? CLIMB-UP> <EQUAL? ,PRSO ,STAIRS>>
		      <DO-WALK ,P?UP>)
		     (<AND <VERB? CLIMB-UP> <EQUAL? ,PRSO ,STAIRS>>
		      <TELL "There are no stairs leading down." CR>)>)>>

<ROUTINE STONE-BARROW-FCN (RARG)
	 <COND (<AND <EQUAL? .RARG ,M-BEG>
		     <OR <VERB? ENTER>
			 <AND <VERB? WALK>
			      <EQUAL? ,PRSO ,P?WEST ,P?IN>>
			 <AND <VERB? THROUGH>
			      <EQUAL? ,PRSO ,BARROW>>>>
		<TELL
"Inside the Barrow|
As you enter the barrow, the door closes inexorably behind you. Around
you it is dark, but ahead is an enormous cavern, brightly lit. Through
its center runs a wide stream. Spanning the stream is a small wooden
footbridge, and beyond a path leads into a dark tunnel. Above the
bridge, floating in the air, is a large sign. It reads:  All ye who
stand before this bridge have completed a great and perilous adventure
which has tested your wit and courage. You have mastered">
		<COND (<EQUAL? <BAND <GETB 0 1> 8> 0>
		       <TELL "
the first part of the ZORK trilogy. Those who pass over this bridge must be
prepared to undertake an even greater adventure that will severely test your
skill and bravery!|
|
The ZORK trilogy continues with \"ZORK II: The Wizard of Frobozz\" and
is completed in \"ZORK III: The Dungeon Master.\"" CR>)
		      (T
		       <TELL "
ZORK: The Great Underground Empire.|" CR>)>
		<FINISH>)>>

<ROUTINE BARROW-DOOR-FCN ()
	 <COND (<VERB? OPEN CLOSE>
		<TELL "The door is too heavy." CR>)>>

<ROUTINE BARROW-FCN ()
	 <COND (<VERB? THROUGH>
		<DO-WALK ,P?WEST>)>>

\

<ROUTINE TROPHY-CASE-FCN ()
    <COND (<AND <VERB? TAKE> <EQUAL? ,PRSO ,TROPHY-CASE>>
	   <TELL
"The trophy case is securely fastened to the wall." CR>)>>

<GLOBAL RUG-MOVED <>>

<ROUTINE LIVING-ROOM-FCN (RARG "AUX" RUG? TC)
	<COND (<EQUAL? .RARG ,M-LOOK>
	       <TELL
"You are in the living room. There is a doorway to the east">
	       <COND (,MAGIC-FLAG
		      <TELL
". To the
west is a cyclops-shaped opening in an old wooden door, above which is
some strange gothic lettering, ">)
		     (T
		      <TELL
", a wooden
door with strange gothic lettering to the west, which appears to be
nailed shut, ">)>
	       <TELL "a trophy case, ">
	       <SET RUG? ,RUG-MOVED>
	       <COND (<AND .RUG? <FSET? ,TRAP-DOOR ,OPENBIT>>
		      <TELL
		       "and a rug lying beside an open trap door.">)
		     (.RUG?
		      <TELL "and a closed trap door at your feet.">)
		     (<FSET? ,TRAP-DOOR ,OPENBIT>
		      <TELL "and an open trap door at your feet.">)
		     (T
		      <TELL
		       "and a large oriental rug in the center of the room.">)>
	       <CRLF>
	       T)
	      (<EQUAL? .RARG ,M-END>
	       <COND (<OR <VERB? TAKE>
			  <AND <VERB? PUT>
			       <EQUAL? ,PRSI ,TROPHY-CASE>>>
		      <COND (<IN? ,PRSO ,TROPHY-CASE>
			     <TOUCH-ALL ,PRSO>)>
		      <SETG SCORE <+ ,BASE-SCORE <OTVAL-FROB>>>
		      <SCORE-UPD 0>
		      <RFALSE>)>)>>

<ROUTINE TOUCH-ALL (OBJ "AUX" F)
	 <SET F <FIRST? .OBJ>>
	 <REPEAT ()
		 <COND (<NOT .F> <RETURN>)
		       (T
			<FSET .F ,TOUCHBIT>
			<COND (<FIRST? .F> <TOUCH-ALL .F>)>)>
		 <SET F <NEXT? .F>>>>

<ROUTINE OTVAL-FROB ("OPTIONAL" (O ,TROPHY-CASE) "AUX" F (SCORE 0))
	 <SET F <FIRST? .O>>
	 <REPEAT ()
		 <COND (<NOT .F> <RETURN .SCORE>)>
		 <SET SCORE <+ .SCORE <GETP .F ,P?TVALUE>>>
		 <COND (<FIRST? .F> <OTVAL-FROB .F>)>
		 <SET F <NEXT? .F>>>>

<ROUTINE TRAP-DOOR-FCN ()
    <COND (<VERB? RAISE>
	   <PERFORM ,V?OPEN ,TRAP-DOOR>
	   <RTRUE>)
	  (<AND <VERB? OPEN CLOSE>
		<EQUAL? ,HERE ,LIVING-ROOM>>
	   <OPEN-CLOSE ,PRSO
"The door reluctantly opens to reveal a rickety staircase descending into
darkness."
"The door swings shut and closes.">)
	  (<AND <VERB? LOOK-UNDER> <EQUAL? ,HERE LIVING-ROOM>>
	   <COND (<FSET? ,TRAP-DOOR ,OPENBIT>
		  <TELL
"You see a rickety staircase descending into darkness." CR>)
		 (T <TELL "It's closed." CR>)>)
	  (<EQUAL? ,HERE ,CELLAR>
	   <COND (<AND <VERB? OPEN UNLOCK>
		       <NOT <FSET? ,TRAP-DOOR ,OPENBIT>>>
		  <TELL
"The door is locked from above." CR>)
		 (<AND <VERB? CLOSE> <NOT <FSET? ,TRAP-DOOR ,OPENBIT>>>
		  <FCLEAR ,TRAP-DOOR ,TOUCHBIT>
		  <FCLEAR ,TRAP-DOOR ,OPENBIT>
		  <TELL "The door closes and locks." CR>)
		 (<VERB? OPEN CLOSE>
		  <TELL <PICK-ONE ,DUMMY> CR>)>)>>

<ROUTINE CELLAR-FCN (RARG)
  <COND (<EQUAL? .RARG ,M-LOOK>
	 <TELL
"You are in a dark and damp cellar with a narrow passageway leading
north, and a crawlway to the south. On the west is the bottom of a
steep metal ramp which is unclimbable." CR>)
	(<EQUAL? .RARG ,M-ENTER>
	 <COND (<AND <FSET? ,TRAP-DOOR ,OPENBIT>
		     <NOT <FSET? ,TRAP-DOOR ,TOUCHBIT>>>
		<FCLEAR ,TRAP-DOOR ,OPENBIT>
		<FSET ,TRAP-DOOR ,TOUCHBIT>
		<TELL
"The trap door crashes shut, and you hear someone barring it." CR CR>)>)>>

<ROUTINE CHIMNEY-F ()
	 <COND (<VERB? EXAMINE>
		<TELL "The chimney leads ">
		<COND (<==? ,HERE ,KITCHEN>
		       <TELL "down">)
		      (T <TELL "up">)>
		<TELL "ward, and looks climbable." CR>)>>

<ROUTINE UP-CHIMNEY-FUNCTION ("AUX" F)
  <COND (<NOT <SET F <FIRST? ,WINNER>>>
	 <TELL "Going up empty-handed is a bad idea." CR>
	 <RFALSE>)
	(<AND <OR <NOT <SET F <NEXT? .F>>>
		  <NOT <NEXT? .F>>>
	      <IN? ,LAMP ,WINNER>>
	 <COND (<NOT <FSET? ,TRAP-DOOR ,OPENBIT>>
		<FCLEAR ,TRAP-DOOR ,TOUCHBIT>)>
	 <RETURN ,KITCHEN>)
	(T
	 <TELL "You can't get up there with what you're carrying." CR>
	 <RFALSE>)>>

<ROUTINE TRAP-DOOR-EXIT ()
	 <COND (,RUG-MOVED
		<COND (<FSET? ,TRAP-DOOR ,OPENBIT>
		       <RETURN ,CELLAR>)
		      (T
		       <TELL "The trap door is closed." CR>
		       <THIS-IS-IT ,TRAP-DOOR>
		       <RFALSE>)>)
	       (T
		<TELL "You can't go that way." CR>
		<RFALSE>)>>

<ROUTINE RUG-FCN ()
   <COND (<VERB? RAISE>
	  <TELL "The rug is too heavy to lift">
	  <COND (,RUG-MOVED
		 <TELL "." CR>)
		(T
		 <TELL
", but in trying to take it you have
noticed an irregularity beneath it." CR>)>)
	 (<VERB? MOVE PUSH>
	  <COND (,RUG-MOVED
		 <TELL
"Having moved the carpet previously, you find it impossible to move
it again." CR>)
		(T
		 <TELL
"With a great effort, the rug is moved to one side of the room, revealing
the dusty cover of a closed trap door." CR>
		 <FCLEAR ,TRAP-DOOR ,INVISIBLE>
		 <THIS-IS-IT ,TRAP-DOOR>
		 <SETG RUG-MOVED T>)>)
	 (<VERB? TAKE>
	  <TELL
"The rug is extremely heavy and cannot be carried." CR>)
	 (<AND <VERB? LOOK-UNDER>
	       <NOT ,RUG-MOVED>
	       <NOT <FSET? ,TRAP-DOOR ,OPENBIT>>>
	  <TELL
"Underneath the rug is a closed trap door. As you drop the corner of the
rug, the trap door is once again concealed from view." CR>)
	 (<VERB? CLIMB-ON>
	  <COND (<AND <NOT ,RUG-MOVED>
		      <NOT <FSET? ,TRAP-DOOR ,OPENBIT>>>
		 <TELL
"As you sit, you notice an irregularity underneath it. Rather than be
uncomfortable, you stand up again." CR>)
		(ELSE
		 <TELL "I suppose you think it's a magic carpet?" CR>)>)>>

\

"SUBTITLE TROLL"

<ROUTINE AXE-F ()
	 <COND (,TROLL-FLAG <>)
	       (T <WEAPON-FUNCTION ,AXE ,TROLL>)>>

<ROUTINE STILETTO-FUNCTION ()
	 <WEAPON-FUNCTION ,STILETTO ,THIEF>>

<ROUTINE WEAPON-FUNCTION (W V)
	<COND (<NOT <IN? .V ,HERE>> <RFALSE>)
	      (<VERB? TAKE>
	       <COND (<IN? .W .V>
		      <TELL
"The " D .V " swings it out of your reach." CR>)
		     (T
		      <TELL
"The " D .W " seems white-hot. You can't hold on to it." CR>)>
	       T)>>

<ROUTINE TROLL-FCN ("OPTIONAL" (MODE <>))
	 <COND (<VERB? TELL>
		<SETG P-CONT <>>
		<TELL "The troll isn't much of a conversationalist." CR>)
	       (<EQUAL? .MODE ,F-BUSY?>
		<COND (<IN? ,AXE ,TROLL> <>)
		      (<AND <IN? ,AXE ,HERE> <PROB 75 90>>
		       <FSET ,AXE ,NDESCBIT>
		       <FCLEAR ,AXE ,WEAPONBIT>
		       <MOVE ,AXE ,TROLL>
		       <PUTP ,TROLL ,P?LDESC
"A nasty-looking troll, brandishing a bloody axe, blocks all passages out
of the room.">
		       <AND <IN? ,TROLL ,HERE>
			    <TELL
"The troll, angered and humiliated, recovers his weapon. He appears to have
an axe to grind with you." CR>>
		      T)
		     (<IN? ,TROLL ,HERE>
		      <PUTP ,TROLL ,P?LDESC
"A pathetically babbling troll is here.">
		      <TELL
"The troll, disarmed, cowers in terror, pleading for his life in
the guttural tongue of the trolls." CR>
		      T)>)
	      (<EQUAL? .MODE ,F-DEAD>
	       <COND (<IN? ,AXE ,TROLL>
		      <MOVE ,AXE ,HERE>
		      <FCLEAR ,AXE ,NDESCBIT>
		      <FSET ,AXE ,WEAPONBIT>)>
	       <SETG TROLL-FLAG T>)
	      (<EQUAL? .MODE ,F-UNCONSCIOUS>
	       <FCLEAR ,TROLL ,FIGHTBIT>
	       <COND (<IN? ,AXE ,TROLL>
		      <MOVE ,AXE ,HERE>
		      <FCLEAR ,AXE ,NDESCBIT>
		      <FSET ,AXE ,WEAPONBIT>)>
	       <PUTP ,TROLL ,P?LDESC
"An unconscious troll is sprawled on the floor. All passages
out of the room are open.">
	       <SETG TROLL-FLAG T>)
	      (<EQUAL? .MODE ,F-CONSCIOUS>
	       <COND (<IN? ,TROLL ,HERE>
		      <FSET ,TROLL ,FIGHTBIT>
		      <TELL
"The troll stirs, quickly resuming a fighting stance." CR>)>
	       <COND (<IN? ,AXE ,TROLL>
		      <PUTP ,TROLL ,P?LDESC
"A nasty-looking troll, brandishing a bloody axe, blocks
all passages out of the room.">)
		     (<IN? ,AXE ,TROLL-ROOM>
		      <FSET ,AXE ,NDESCBIT>
		      <FCLEAR ,AXE ,WEAPONBIT>
		      <MOVE ,AXE ,TROLL>
		      <PUTP ,TROLL ,P?LDESC
"A nasty-looking troll, brandishing a bloody axe, blocks
all passages out of the room.">)
		     (T
		      <PUTP ,TROLL ,P?LDESC
"A troll is here.">)>
	       <SETG TROLL-FLAG <>>)
	      (<EQUAL? .MODE ,F-FIRST?>
	       <COND (<PROB 33>
		      <FSET ,TROLL ,FIGHTBIT>
		      <SETG P-CONT <>>
		      T)>)
	      (<NOT .MODE>
	       <COND (<VERB? EXAMINE>
		      <TELL <GETP ,TROLL ,P?LDESC> CR>)
		     (<OR <AND <VERB? THROW GIVE>
			       ,PRSO
			       <EQUAL? ,PRSI ,TROLL>>
			  <VERB? TAKE MOVE MUNG>>
		      <AWAKEN ,TROLL>
		      <COND (<VERB? THROW GIVE>
			     <COND (<AND <EQUAL? ,PRSO ,AXE>
					 <IN? ,AXE ,WINNER>>
				    <TELL
"The troll scratches his head in confusion, then takes the axe." CR>
				    <FSET ,TROLL ,FIGHTBIT>
				    <MOVE ,AXE ,TROLL>
				    <RTRUE>)
				   (<EQUAL? ,PRSO ,TROLL ,AXE>
				    <TELL
"You would have to get the " D ,PRSO " first, and that seems unlikely." CR>
				    <RTRUE>)>
			     <COND (<VERB? THROW>
				    <TELL
"The troll, who is remarkably coordinated, catches the " D ,PRSO>)
				   (T
				    <TELL
"The troll, who is not overly proud, graciously accepts the gift">)>
			     <COND (<AND <PROB 20>
					 <EQUAL? ,PRSO ,KNIFE ,SWORD ,AXE>>
				    <REMOVE-CAREFULLY ,PRSO>
				    <TELL
" and eats it hungrily. Poor troll, he dies from an internal hemorrhage
and his carcass disappears in a sinister black fog." CR>
				    <REMOVE-CAREFULLY ,TROLL>
				    <APPLY <GETP ,TROLL ,P?ACTION> ,F-DEAD>
				    <SETG TROLL-FLAG T>)
				   (<EQUAL? ,PRSO ,KNIFE ,SWORD ,AXE>
				    <MOVE ,PRSO ,HERE>
				    <TELL
" and, being for the moment sated, throws it back. Fortunately, the
troll has poor control, and the " D ,PRSO " falls to the floor. He does
not look pleased." CR>
				    <FSET ,TROLL ,FIGHTBIT>)
				   (T
				    <TELL
" and not having the most discriminating tastes, gleefully eats it." CR>
				    <REMOVE-CAREFULLY ,PRSO>)>)
			    (<VERB? TAKE MOVE>
			     <TELL
"The troll spits in your face, grunting \"Better luck next time\" in a
rather barbarous accent." CR>)
			    (<VERB? MUNG>
			     <TELL
"The troll laughs at your puny gesture." CR>)>)
		     (<VERB? LISTEN>
		      <TELL
"Every so often the troll says something, probably uncomplimentary, in
his guttural tongue." CR>)
		     (<AND ,TROLL-FLAG <VERB? HELLO>>
		      <TELL "Unfortunately, the troll can't hear you." CR>)>)>>

\

"SUBTITLE GRATING/MAZE"

;<GLOBAL LEAVES-GONE <>> ;"no longer used?"
<GLOBAL GRATE-REVEALED <>>
<GLOBAL GRUNLOCK <>>

<ROUTINE LEAVES-APPEAR ()
	<COND (<AND <NOT <FSET? ,GRATE ,OPENBIT>>
	            <NOT ,GRATE-REVEALED>>
	       <COND (<VERB? MOVE TAKE>
		      <TELL
"In disturbing the pile of leaves, a grating is revealed." CR>)
		     (T <TELL
"With the leaves moved, a grating is revealed." CR>)>
	       <FCLEAR ,GRATE ,INVISIBLE>
	       <SETG GRATE-REVEALED T>)>
	<>>

<ROUTINE LEAF-PILE ()
	<COND (<VERB? COUNT>
	       <TELL "There are 69,105 leaves here." CR>)
	      (<VERB? BURN>
	       <LEAVES-APPEAR>
	       <REMOVE-CAREFULLY ,PRSO>
	       <COND (<IN? ,PRSO ,HERE>
		      <TELL
"The leaves burn." CR>)
		     (T
		      <JIGS-UP
"The leaves burn, and so do you.">)>)
	      (<VERB? CUT>
	       <TELL "You rustle the leaves around, making quite a mess." CR>
	       <LEAVES-APPEAR>
	       <RTRUE>)
	      (<VERB? MOVE TAKE>
	       <COND (<VERB? MOVE>
		      <TELL "Done." CR>)>
	       <COND (,GRATE-REVEALED <RFALSE>)>
	       <LEAVES-APPEAR>
	       <COND (<VERB? TAKE> <RFALSE>)
		     (T <RTRUE>)>)
	      (<AND <VERB? LOOK-UNDER>
		    <NOT ,GRATE-REVEALED>>
	       <TELL
"Underneath the pile of leaves is a grating. As you release the leaves,
the grating is once again concealed from view." CR>)>>

<ROUTINE CLEARING-FCN (RARG)
  	 <COND (<EQUAL? .RARG ,M-ENTER>
		<COND (<NOT ,GRATE-REVEALED>
		       <FSET ,GRATE ,INVISIBLE>)>)
	       (<EQUAL? .RARG ,M-LOOK>
		<TELL
"You are in a clearing, with a forest surrounding you on all sides. A
path leads south.">
		<COND (<FSET? ,GRATE ,OPENBIT>
		       <CRLF>
		       <TELL
"There is an open grating, descending into darkness.">)
		      (,GRATE-REVEALED
		       <CRLF>
		       <TELL
"There is a grating securely fastened into the ground.">)>
		<CRLF>)>>

<ROUTINE MAZE-11-FCN (RARG)
  	 <COND (<EQUAL? .RARG ,M-ENTER>
		<FCLEAR ,GRATE ,INVISIBLE>)
	       (<EQUAL? .RARG ,M-LOOK>
		<TELL
"You are in a small room near the maze. There are twisty passages
in the immediate vicinity." CR>
		<COND (<FSET? ,GRATE ,OPENBIT>
		       <TELL
 "Above you is an open grating with sunlight pouring in.">)
		      (,GRUNLOCK
		       <TELL "Above you is a grating.">)
		      (T
		       <TELL
 "Above you is a grating locked with a skull-and-crossbones lock.">)>
		<CRLF>)>>

<ROUTINE GRATE-FUNCTION ()
    	 <COND (<AND <VERB? OPEN> <EQUAL? ,PRSI ,KEYS>>
		<PERFORM ,V?UNLOCK ,GRATE ,KEYS>
		<RTRUE>)
	       (<VERB? LOCK>
		<COND (<EQUAL? ,HERE ,GRATING-ROOM>
		       <SETG GRUNLOCK <>>
		       <TELL "The grate is locked." CR>)
	              (<EQUAL? ,HERE ,GRATING-CLEARING>
		       <TELL "You can't lock it from this side." CR>)>)
	       (<AND <VERB? UNLOCK> <EQUAL? ,PRSO ,GRATE>>
		<COND (<AND <EQUAL? ,HERE ,GRATING-ROOM> <EQUAL? ,PRSI ,KEYS>>
		       <SETG GRUNLOCK T>
		       <TELL "The grate is unlocked." CR>)
		      (<AND <EQUAL? ,HERE ,GRATING-CLEARING>
			    <EQUAL? ,PRSI ,KEYS>>
		       <TELL "You can't reach the lock from here." CR>)
		      (T
		       <TELL
"Can you unlock a grating with a " D ,PRSI "?" CR>)>)
               (<VERB? PICK>
		<TELL "You can't pick the lock." CR>)
               (<VERB? OPEN CLOSE>
		<COND (,GRUNLOCK
		       <OPEN-CLOSE ,GRATE
				   <COND (<EQUAL? ,HERE ,CLEARING>
					  "The grating opens.")
					 (T
"The grating opens to reveal trees above you.")>
				   "The grating is closed.">
		       <COND (<FSET? ,GRATE ,OPENBIT>
			      <COND (<AND <NOT <EQUAL? ,HERE ,CLEARING>>
					  <NOT ,GRATE-REVEALED>>
				     <TELL
"A pile of leaves falls onto your head and to the ground." CR>
				     <SETG GRATE-REVEALED T>
				     <MOVE ,LEAVES ,HERE>)>
			      <FSET ,GRATING-ROOM ,ONBIT>)
			     (T <FCLEAR ,GRATING-ROOM ,ONBIT>)>)
		      (T <TELL "The grating is locked." CR>)>)
	       (<AND <VERB? PUT> <EQUAL? ,PRSI ,GRATE>>
		<COND (<G? <GETP ,PRSO ,P?SIZE> 20>
		       <TELL "It won't fit through the grating." CR>)
		      (T
		       <MOVE ,PRSO ,GRATING-ROOM>
		       <TELL
"The " D ,PRSO " goes through the grating into the darkness below." CR>)>)>>

<ROUTINE MAZE-DIODES ()
	 <TELL
"You won't be able to get back up to the tunnel you are going through
when it gets to the next room." CR CR>
	 <COND (<EQUAL? ,HERE ,MAZE-2> ,MAZE-4)
	       (<EQUAL? ,HERE ,MAZE-7> ,DEAD-END-1)
	       (<EQUAL? ,HERE ,MAZE-9> ,MAZE-11)
	       (<EQUAL? ,HERE ,MAZE-12> ,MAZE-5)>>

<ROUTINE RUSTY-KNIFE-FCN ()
	<COND (<VERB? TAKE>
	       <AND <IN? ,SWORD ,WINNER>
		    <TELL
"As you touch the rusty knife, your sword gives a single pulse of blinding
blue light." CR>>
	       <>)
	      (<OR <AND <EQUAL? ,PRSI ,RUSTY-KNIFE>
			<VERB? ATTACK>>
		   <AND <VERB? SWING>
			<EQUAL? ,PRSO ,RUSTY-KNIFE>
			,PRSI>>
	       <REMOVE-CAREFULLY ,RUSTY-KNIFE>
	       <JIGS-UP
"As the knife approaches its victim, your mind is submerged by an
overmastering will. Slowly, your hand turns, until the rusty blade
is an inch from your neck. The knife seems to sing as it savagely
slits your throat.">)>>

<ROUTINE KNIFE-F ()
	 <COND (<VERB? TAKE>
		<FCLEAR ,ATTIC-TABLE ,NDESCBIT>
		<RFALSE>)>>

<ROUTINE SKELETON ()
	 <COND (<VERB? TAKE RUB MOVE PUSH RAISE LOWER ATTACK KICK KISS>
		<TELL
"A ghost appears in the room and is appalled at your desecration of
the remains of a fellow adventurer. He casts a curse on your valuables
and banishes them to the Land of the Living Dead. The ghost leaves,
muttering obscenities." CR>
	 	<ROB ,HERE ,LAND-OF-LIVING-DEAD 100>
	 	<ROB ,ADVENTURER ,LAND-OF-LIVING-DEAD>
	 	T)>>

\

<ROUTINE TORCH-OBJECT ()
    <COND (<VERB? EXAMINE>
	   <TELL "The torch is burning." CR>)
	  (<AND <VERB? POUR-ON>
		<EQUAL? ,PRSI ,TORCH>>
	   <TELL "The water evaporates before it gets close." CR>)
	  (<AND <VERB? LAMP-OFF> <FSET? ,PRSO ,ONBIT>>
	   <TELL
"You nearly burn your hand trying to extinguish the flame." CR>)>>

\

"SUBTITLE MIRROR, MIRROR, ON THE WALL"

<ROUTINE MIRROR-ROOM (RARG)
	 <COND (<EQUAL? .RARG ,M-LOOK>
	        <TELL
"You are in a large square room with tall ceilings. On the south wall
is an enormous mirror which fills the entire wall. There are exits
on the other three sides of the room." CR>
		<COND (,MIRROR-MUNG
		       <TELL
"Unfortunately, the mirror has been destroyed by your recklessness." CR>)>)>>

<GLOBAL MIRROR-MUNG <>>
<GLOBAL LUCKY T>

<ROUTINE MIRROR-MIRROR ("AUX" (RM2 ,MIRROR-ROOM-2) L1 L2 N)
	<COND (<AND <NOT ,MIRROR-MUNG> <VERB? RUB>>
	       <COND (<AND ,PRSI <NOT <EQUAL? ,PRSI ,HANDS>>>
		      <TELL
"You feel a faint tingling transmitted through the " D ,PRSI "." CR>
		      <RTRUE>)>
	       <COND (<EQUAL? ,HERE .RM2>
		      <SET RM2 ,MIRROR-ROOM-1>)>
	       <SET L1 <FIRST? ,HERE>>
	       <SET L2 <FIRST? .RM2>>
	       <REPEAT ()
		       <COND (<NOT .L1> <RETURN>)>
		       <SET N <NEXT? .L1>>
		       <MOVE .L1 .RM2>
		       <SET L1 .N>>
	       <REPEAT ()
		       <COND (<NOT .L2> <RETURN>)>
		       <SET N <NEXT? .L2>>
		       <MOVE .L2 ,HERE>
		       <SET L2 .N>>
	       <GOTO .RM2 <>>
	       <TELL
"There is a rumble from deep within the earth and the room shakes." CR>)
	      (<VERB? LOOK-INSIDE EXAMINE>
	       <COND (,MIRROR-MUNG
		      <TELL "The mirror is broken into many pieces.">)
		     (T
		      <TELL "There is an ugly person staring back at you.">)>
	       <CRLF>)
	      (<VERB? TAKE>
	       <TELL
"The mirror is many times your size. Give up." CR>)
	      (<VERB? MUNG THROW ATTACK>
	       <COND (,MIRROR-MUNG
		      <TELL
"Haven't you done enough damage already?" CR>)
		     (T
		      <SETG MIRROR-MUNG T>
		      <SETG LUCKY <>>
		      <TELL
"You have broken the mirror. I hope you have a seven years' supply of
good luck handy." CR>)>)>>

\

"SUBTITLE THE DOME"

<ROUTINE TORCH-ROOM-FCN (RARG)
 	 <COND (<EQUAL? .RARG ,M-LOOK>
		<TELL
"This is a large room with a prominent doorway leading to a down
staircase. Above you is a large dome. Up around the edge of the
dome (20 feet up) is a wooden railing. In the center of the room
sits a white marble pedestal." CR>
		<COND (,DOME-FLAG
		       <TELL
"A piece of rope descends from the railing above, ending some
five feet above your head." CR>)>)>>

<ROUTINE DOME-ROOM-FCN (RARG)
	 <COND (<EQUAL? .RARG ,M-LOOK>
		<TELL
"You are at the periphery of a large dome, which forms the ceiling
of another room below. Protecting you from a precipitous drop is a
wooden railing which circles the dome." CR>
		<COND (,DOME-FLAG
		       <TELL
"Hanging down from the railing is a rope which ends about ten feet
from the floor below." CR>)>)
	       (<EQUAL? .RARG ,M-ENTER>
		<COND (,DEAD
		       <TELL
"As you enter the dome you feel a strong pull as if from a wind
drawing you over the railing and down." CR>
		       <MOVE ,WINNER ,TORCH-ROOM>
		       <SETG HERE ,TORCH-ROOM>
		       <RTRUE>)
		      (<VERB? LEAP>
		       <JIGS-UP
"I'm afraid that the leap you attempted has done you in.">)>)>>

;<GLOBAL EGYPT-FLAG <>>	;"no longer used?"

\

"SUBTITLE LAND OF THE DEAD"

<ROUTINE LLD-ROOM (RARG)
	 <COND (<EQUAL? .RARG ,M-LOOK>
		<TELL
"You are outside a large gateway, on which is inscribed||
  Abandon every hope
all ye who enter here!||
The gate is open; through it you can see a desolation, with a pile of
mangled bodies in one corner. Thousands of voices, lamenting some
hideous fate, can be heard." CR>
		<COND (<AND <NOT ,LLD-FLAG> <NOT ,DEAD>>
		       <TELL
"The way through the gate is barred by evil spirits, who jeer at your
attempts to pass." CR>)>)
	       (<EQUAL? .RARG ,M-BEG>
		<COND (<VERB? EXORCISE>
		       <COND (<NOT ,LLD-FLAG>
			      <COND (<AND <IN? ,BELL ,WINNER>
					  <IN? ,BOOK ,WINNER>
					  <IN? ,CANDLES ,WINNER>>
				     <TELL
"You must perform the ceremony." CR>)
				    (T
				     <TELL
"You aren't equipped for an exorcism." CR>)>)>)
		      (<AND <NOT ,LLD-FLAG>
			    <VERB? RING>
			    <EQUAL? ,PRSO ,BELL>>
		       <SETG XB T>
		       <REMOVE-CAREFULLY ,BELL>
		       <THIS-IS-IT ,HOT-BELL>
		       <MOVE ,HOT-BELL ,HERE>
		       <TELL
"The bell suddenly becomes red hot and falls to the ground. The
wraiths, as if paralyzed, stop their jeering and slowly turn to face
you. On their ashen faces, the expression of a long-forgotten terror
takes shape." CR>
		       <COND (<IN? ,CANDLES ,WINNER>
			      <TELL
"In your confusion, the candles drop to the ground (and they are out)." CR>
			      <MOVE ,CANDLES ,HERE>
			      <FCLEAR ,CANDLES ,ONBIT>
			      <DISABLE <INT I-CANDLES>>)>
		       <ENABLE <QUEUE I-XB 6>>
		       <ENABLE <QUEUE I-XBH 20>>)
		      (<AND ,XC
			    <VERB? READ>
			    <EQUAL? ,PRSO ,BOOK>
			    <NOT ,LLD-FLAG>>
		       <TELL
"Each word of the prayer reverberates through the hall in a deafening
confusion. As the last word fades, a voice, loud and commanding,
speaks: \"Begone, fiends!\" A heart-stopping scream fills the cavern,
and the spirits, sensing a greater power, flee through the walls." CR>
		       <REMOVE-CAREFULLY ,GHOSTS>
		       <SETG LLD-FLAG T>
		       <DISABLE <INT I-XC>>)>)
	       (<EQUAL? .RARG ,M-END>
		<COND (<AND ,XB
			    <IN? ,CANDLES ,WINNER>
			    <FSET? ,CANDLES ,ONBIT>
			    <NOT ,XC>>
		       <SETG XC T>
		       <TELL
"The flames flicker wildly and appear to dance. The earth beneath
your feet trembles, and your legs nearly buckle beneath you.
The spirits cower at your unearthly power." CR>
		       <DISABLE <INT I-XB>>
		       <ENABLE <QUEUE I-XC 3>>)>)>>

<GLOBAL XB <>>

<GLOBAL XC <>>

<ROUTINE I-XB ()
	 <OR ,XC
	     <AND <EQUAL? ,HERE ,ENTRANCE-TO-HADES>
		  <TELL
"The tension of this ceremony is broken, and the wraiths, amused but
shaken at your clumsy attempt, resume their hideous jeering." CR>>>
	 <SETG XB <>>>

<ROUTINE I-XC ()
	 <SETG XC <>>
	 <I-XB>>

<ROUTINE I-XBH ()
	 <REMOVE-CAREFULLY ,HOT-BELL>
	 <MOVE ,BELL ,ENTRANCE-TO-HADES>
	 <COND (<EQUAL? ,HERE ,ENTRANCE-TO-HADES>
		<TELL "The bell appears to have cooled down." CR>)>>

\

"SUBTITLE FLOOD CONTROL DAM #3"

<GLOBAL GATE-FLAG <>>
<GLOBAL GATES-OPEN <>>

<ROUTINE DAM-ROOM-FCN (RARG)
   	 <COND (<EQUAL? .RARG ,M-LOOK>
		<TELL
"You are standing on the top of the Flood Control Dam #3, which was
quite a tourist attraction in times far distant. There are paths to
the north, south, and west, and a scramble down." CR>
		<COND (<AND ,LOW-TIDE ,GATES-OPEN>
		       <TELL
"The water level behind the dam is low: The sluice gates have been
opened. Water rushes through the dam and downstream." CR>)
		      (,GATES-OPEN
		       <TELL
"The sluice gates are open, and water rushes through the dam. The
water level behind the dam is still high." CR>)
		      (,LOW-TIDE
		       <TELL
"The sluice gates are closed. The water level in the reservoir is
quite low, but the level is rising quickly." CR>)
		      (T
		       <TELL
"The sluice gates on the dam are closed. Behind the dam, there can be
seen a wide reservoir. Water is pouring over the top of the now
abandoned dam." CR>)>
		<TELL
"There is a control panel here, on which a large metal bolt is mounted.
Directly above the bolt is a small green plastic bubble">
		<COND (,GATE-FLAG
		       <TELL " which is
glowing serenely">)>
		<TELL "." CR>)>>

<ROUTINE BOLT-F ()
	<COND (<VERB? TURN>
	       <COND (<EQUAL? ,PRSI ,WRENCH>
		      <COND (,GATE-FLAG
			     <FCLEAR ,RESERVOIR-SOUTH ,TOUCHBIT>
			     <COND (,GATES-OPEN
				    <SETG GATES-OPEN <>>
				    <FCLEAR ,LOUD-ROOM ,TOUCHBIT>
				    <TELL
"The sluice gates close and water starts to collect behind the dam." CR>
				    <ENABLE <QUEUE I-RFILL 8>>
				    <QUEUE I-REMPTY 0>
				    T)
				   (T
				    <SETG GATES-OPEN T>
				    <TELL
"The sluice gates open and water pours through the dam." CR>
				    <ENABLE <QUEUE I-REMPTY 8>>
				    <QUEUE I-RFILL 0>
				    T)>)
			    (T <TELL
"The bolt won't turn with your best effort." CR>)>)
		     (ELSE
		      <TELL
"The bolt won't turn using the " D ,PRSI "." CR>)>)
	      (<VERB? TAKE>
	       <INTEGRAL-PART>)
	      (<VERB? OIL>
	       <TELL
"Hmm. It appears the tube contained glue, not oil. Turning the bolt
won't get any easier...." CR>)>>

<ROUTINE BUBBLE-F ()
	 <COND (<VERB? TAKE>
		<INTEGRAL-PART>)>>

<ROUTINE INTEGRAL-PART ()
	 <TELL "It is an integral part of the control panel." CR>>

<ROUTINE I-RFILL ()
	 <FSET ,RESERVOIR ,NONLANDBIT>
	 <FCLEAR ,RESERVOIR ,RLANDBIT>
	 <FCLEAR ,DEEP-CANYON ,TOUCHBIT>
	 <FCLEAR ,LOUD-ROOM ,TOUCHBIT>
	 <AND <IN? ,TRUNK ,RESERVOIR>
	      <FSET ,TRUNK ,INVISIBLE>>
	 <SETG LOW-TIDE <>>
	 <COND (<EQUAL? ,HERE ,RESERVOIR>
		<COND (<FSET? <LOC ,WINNER> ,VEHBIT>
		       <TELL
"The boat lifts gently out of the mud and is now floating on
the reservoir." CR>)
		      (T
		       <JIGS-UP
"You are lifted up by the rising river! You try to swim, but the
currents are too strong. You come closer, closer to the awesome
structure of Flood Control Dam #3. The dam beckons to you.
The roar of the water nearly deafens you, but you remain conscious
as you tumble over the dam toward your certain doom among the rocks
at its base.">)>)
	       (<EQUAL? ,HERE ,DEEP-CANYON>
		<TELL
"A sound, like that of flowing water, starts to come from below." CR>)
	       (<EQUAL? ,HERE ,LOUD-ROOM>
		<TELL
"All of a sudden, an alarmingly loud roaring sound fills the room.
Filled with fear, you scramble away." CR>
		<GOTO <PICK-ONE ,LOUD-RUNS>>)
	       (<EQUAL? ,HERE ,RESERVOIR-NORTH ,RESERVOIR-SOUTH>
		<TELL
"You notice that the water level has risen to the point that it
is impossible to cross." CR>)>
	 T>

<GLOBAL LOUD-RUNS <LTABLE 0 DAMP-CAVE ROUND-ROOM DEEP-CANYON>>

<ROUTINE I-REMPTY ()
	 <FSET ,RESERVOIR ,RLANDBIT>
	 <FCLEAR ,RESERVOIR ,NONLANDBIT>
	 <FCLEAR ,DEEP-CANYON ,TOUCHBIT>
	 <FCLEAR ,LOUD-ROOM ,TOUCHBIT>
	 <FCLEAR ,TRUNK ,INVISIBLE>
	 <SETG LOW-TIDE T>
	 <COND (<AND <EQUAL? ,HERE ,RESERVOIR>
		     <FSET? <LOC ,WINNER> ,VEHBIT>>
		<TELL
"The water level has dropped to the point at which the boat can no
longer stay afloat. It sinks into the mud." CR>)
	       (<EQUAL? ,HERE ,DEEP-CANYON>
		<TELL
"The roar of rushing water is quieter now." CR>)
	       (<EQUAL? ,HERE ,RESERVOIR-NORTH ,RESERVOIR-SOUTH>
		<TELL
"The water level is now quite low here and you could easily cross over
to the other side." CR>)>
	 T>

<GLOBAL DROWNINGS
      <TABLE (PURE) "up to your ankles."
	"up to your shin."
	"up to your knees."
	"up to your hips."
	"up to your waist."
	"up to your chest."
	"up to your neck."
	"over your head."
	"high in your lungs.">>

<GLOBAL WATER-LEVEL 0>
<GDECL (WATER-LEVEL) FIX>

<ROUTINE BUTTON-F ()
	 <COND (<VERB? READ>
		<TELL "They're greek to you." CR>)
	       (<VERB? PUSH>
		<COND (<EQUAL? ,PRSO ,BLUE-BUTTON>
		       <COND (<0? ,WATER-LEVEL>
			      <FCLEAR ,LEAK ,INVISIBLE>
			      <TELL
"There is a rumbling sound and a stream of water appears to burst
from the east wall of the room (apparently, a leak has occurred in a
pipe)." CR>
			      <SETG WATER-LEVEL 1>
			      <ENABLE <QUEUE I-MAINT-ROOM -1>>
			      T)
			     (T
			      <TELL
			        "The blue button appears to be jammed." CR>)>)
		      (<EQUAL? ,PRSO ,RED-BUTTON>
		       <TELL "The lights within the room ">
		       <COND (<FSET? ,HERE ,ONBIT>
			      <FCLEAR ,HERE ,ONBIT>
			      <TELL "shut off." CR>)
			     (T
			      <FSET ,HERE ,ONBIT>
			      <TELL "come on." CR>)>)
		      (<EQUAL? ,PRSO ,BROWN-BUTTON>
		       <FCLEAR ,DAM-ROOM ,TOUCHBIT>
		       <SETG GATE-FLAG <>>
		       <TELL "Click." CR>)
		      (<EQUAL? ,PRSO ,YELLOW-BUTTON>
		       <FCLEAR ,DAM-ROOM ,TOUCHBIT>
		       <SETG GATE-FLAG T>
		       <TELL "Click." CR>)>)>>

<ROUTINE TOOL-CHEST-FCN ()
	 <COND (<VERB? EXAMINE>
		<TELL "The chests are all empty." CR>)
	       (<VERB? TAKE OPEN PUT>
		<REMOVE-CAREFULLY ,TOOL-CHEST>
<TELL
"The chests are so rusty and corroded that they crumble when you
touch them." CR>)
	       (<VERB? OPEN>
		<TELL "The chests are already open." CR>)>>

<ROUTINE I-MAINT-ROOM ("AUX" HERE?)
	 <SET HERE? <EQUAL? ,HERE ,MAINTENANCE-ROOM>>
	 <COND (.HERE? <TELL "The water level here is now "> <TELL <GET
		,DROWNINGS </ ,WATER-LEVEL 2>>> <CRLF>)>
	 <SETG WATER-LEVEL <+ 1 ,WATER-LEVEL>>
	 <COND (<NOT <L? ,WATER-LEVEL 14>>
		<MUNG-ROOM ,MAINTENANCE-ROOM
"The room is full of water and cannot be entered.">
		<QUEUE I-MAINT-ROOM 0>
		<COND (.HERE?
		     <JIGS-UP
"I'm afraid you have done drowned yourself.">)>)
	       (<AND <IN? ,WINNER ,INFLATED-BOAT>
		     <EQUAL? ,HERE ,MAINTENANCE-ROOM ,DAM-ROOM ,DAM-LOBBY>>
		<JIGS-UP
"The rising water carries the boat over the dam, down the river, and over
the falls. Tsk, tsk.">)>
	 <RTRUE>>

<ROUTINE LEAK-FUNCTION ()
	<COND (<G? ,WATER-LEVEL 0>
	       <COND (<AND <VERB? PUT PUT-ON>
			   <EQUAL? ,PRSO ,PUTTY>>
		      <FIX-MAINT-LEAK>)
		     (<VERB? PLUG>
		      <COND (<EQUAL? ,PRSI ,PUTTY>
			     <FIX-MAINT-LEAK>)
			    (T <WITH-TELL ,PRSI>)>)>)>>

<ROUTINE FIX-MAINT-LEAK ()
	 <SETG WATER-LEVEL -1>
	 <QUEUE I-MAINT-ROOM 0>
	 <TELL
"By some miracle of Zorkian technology, you have managed to stop the
leak in the dam." CR>>

<ROUTINE PUTTY-FCN ()
	 <COND (<OR <AND <VERB? OIL>
			 <EQUAL? ,PRSI ,PUTTY>>
		    <AND <VERB? PUT>
			 <EQUAL? ,PRSO ,PUTTY>>>
		<TELL "The all-purpose gunk isn't a lubricant." CR>)>>

<ROUTINE TUBE-FUNCTION ()
	 <COND (<AND <VERB? PUT>
		     <EQUAL? ,PRSI ,TUBE>>
		<TELL "The tube refuses to accept anything." CR>)
	       (<VERB? SQUEEZE>
		<COND (<AND <FSET? ,PRSO ,OPENBIT>
			    <IN? ,PUTTY ,PRSO>>
		       <MOVE ,PUTTY ,WINNER>
		       <TELL "The viscous material oozes into your hand." CR>)
		      (<FSET? ,PRSO ,OPENBIT>
		       <TELL "The tube is apparently empty." CR>)
		      (T
		       <TELL "The tube is closed." CR>)>)>>

<ROUTINE DAM-FUNCTION ()
	 <COND (<VERB? OPEN CLOSE>
		<TELL "Sounds reasonable, but this isn't how." CR>)
	       (<VERB? PLUG>
		<COND (<EQUAL? ,PRSI ,HANDS>
		       <TELL
"Are you the little Dutch boy, then? Sorry, this is a big dam." CR>)
		      (T
		       <TELL
"With a " D ,PRSI "? Do you know how big this dam is? You could only
stop a tiny leak with that." CR>)>)>>

<ROUTINE WITH-TELL (OBJ)
	 <TELL "With a " D .OBJ "?" CR>>

<ROUTINE RESERVOIR-SOUTH-FCN (RARG)
	<COND (<EQUAL? .RARG ,M-LOOK>
	       <COND (<AND ,LOW-TIDE ,GATES-OPEN>
		      <TELL
"You are in a long room, to the north of which was formerly a lake.
However, with the water level lowered, there is merely a wide stream
running through the center of the room.">)
		     (,GATES-OPEN
		      <TELL
"You are in a long room. To the north is a large lake, too deep
to cross. You notice, however, that the water level appears to be
dropping at a rapid rate. Before long, it might be possible to cross
to the other side from here.">)
		     (,LOW-TIDE
		      <TELL
"You are in a long room, to the north of which is a wide area which
was formerly a reservoir, but now is merely a stream. You notice,
however, that the level of the stream is rising quickly and that
before long it will be impossible to cross here.">)
		     (T
		      <TELL
"You are in a long room on the south shore of a large lake, far
too deep and wide for crossing.">)>
	       <CRLF>
	       <TELL
"There is a path along the stream to the east or west, a steep pathway
climbing southwest along the edge of a chasm, and a path leading into a
canyon to the southeast." CR>)>>

<ROUTINE RESERVOIR-FCN (RARG)
   	<COND (<AND <EQUAL? .RARG ,M-END>
		    <NOT <FSET? <LOC ,WINNER> ,VEHBIT>>
		    <NOT ,GATES-OPEN>
		    ,LOW-TIDE>
	       <TELL
"You notice that the water level here is rising rapidly. The currents
are also becoming stronger. Staying here seems quite perilous!" CR>)
	      (<EQUAL? .RARG ,M-LOOK>
	       <COND (,LOW-TIDE
		      <TELL
"You are on what used to be a large lake, but which is now a large
mud pile. There are \"shores\" to the north and south.">)
		     (T
		      <TELL
"You are on the lake. Beaches can be seen north and south.
Upstream a small stream enters the lake through a narrow cleft
in the rocks. The dam can be seen downstream.">)>
	       <CRLF>)>>

<ROUTINE RESERVOIR-NORTH-FCN (RARG)
	<COND (<EQUAL? .RARG ,M-LOOK>
	       <COND (<AND ,LOW-TIDE ,GATES-OPEN>
		      <TELL
"You are in a large cavernous room, the south of which was formerly
a lake. However, with the water level lowered, there is merely
a wide stream running through there.">)
		     (,GATES-OPEN
		      <TELL
"You are in a large cavernous area. To the south is a wide lake,
whose water level appears to be falling rapidly.">)
		     (,LOW-TIDE
		      <TELL
"You are in a cavernous area, to the south of which is a very wide
stream. The level of the stream is rising rapidly, and it appears
that before long it will be impossible to cross to the other side.">)
		     (T
		      <TELL
"You are in a large cavernous room, north of a large lake.">)>
	       <CRLF>
	       <TELL
"There is a slimy stairway leaving the room to the north." CR>)>>

\

"SUBTITLE WATER, WATER EVERYWHERE..."

<ROUTINE BOTTLE-FUNCTION ("AUX" (E? <>))
  <COND (<AND <VERB? THROW> <==? ,PRSO ,BOTTLE>>
	 <REMOVE-CAREFULLY ,PRSO>
	 <SET E? T>
	 <TELL "The bottle hits the far wall and shatters." CR>)
	(<VERB? MUNG>
	 <SET E? T>
	 <REMOVE-CAREFULLY ,PRSO>
	 <TELL "A brilliant maneuver destroys the bottle." CR>)
	(<VERB? SHAKE>
	 <COND (<AND <FSET? ,PRSO ,OPENBIT> <IN? ,WATER ,PRSO>>
		<SET E? T>)>)>
  <COND (<AND .E? <IN? ,WATER ,PRSO>>
	 <TELL "The water spills to the floor and evaporates." CR>
	 <REMOVE-CAREFULLY ,WATER>
	 T)
	(.E? <RTRUE>)>>

\

"SUBTITLE CYCLOPS"

<GLOBAL CYCLOWRATH 0>

<ROUTINE CYCLOPS-FCN ("AUX" COUNT)
	<SET COUNT ,CYCLOWRATH>
	<COND (<EQUAL? ,WINNER ,CYCLOPS>
	       <COND (,CYCLOPS-FLAG
		      <TELL "No use talking to him. He's fast asleep." CR>)
		     (<VERB? ODYSSEUS>
		      <SETG WINNER ,ADVENTURER>
		      <PERFORM ,V?ODYSSEUS>
		      <RTRUE>)
		     (ELSE
		      <TELL
"The cyclops prefers eating to making conversation." CR>)>)
	      (,CYCLOPS-FLAG
	       <COND (<VERB? EXAMINE>
		      <TELL
"The cyclops is sleeping like a baby, albeit a very ugly one." CR>)
		     (<VERB? ALARM KICK ATTACK BURN MUNG>
		      <TELL
"The cyclops yawns and stares at the thing that woke him up." CR>
		      <SETG CYCLOPS-FLAG <>>
		      <FSET ,CYCLOPS ,FIGHTBIT>
		      <COND (<L? .COUNT 0>
			     <SETG CYCLOWRATH <- .COUNT>>)
			    (T
			     <SETG CYCLOWRATH .COUNT>)>)>)
	      (<VERB? EXAMINE>
	       <TELL
"A hungry cyclops is standing at the foot of the stairs." CR>)
	      (<AND <VERB? GIVE> <EQUAL? ,PRSI ,CYCLOPS>>
	       <COND (<EQUAL? ,PRSO ,LUNCH>
		      <COND (<NOT <L? .COUNT 0>>
			     <REMOVE-CAREFULLY ,LUNCH>
			     <TELL
"The cyclops says \"Mmm Mmm. I love hot peppers! But oh, could I use
a drink. Perhaps I could drink the blood of that thing.\"  From the
gleam in his eye, it could be surmised that you are \"that thing\"." CR>
			     <SETG CYCLOWRATH <MIN -1 <- .COUNT>>>)>
		      <ENABLE <QUEUE I-CYCLOPS -1>>)
		     (<OR <EQUAL? ,PRSO ,WATER>
			  <AND <EQUAL? ,PRSO ,BOTTLE>
			       <IN? ,WATER ,BOTTLE>>>
		      <COND (<L? .COUNT 0>
			     <REMOVE-CAREFULLY ,WATER>
			     <MOVE ,BOTTLE ,HERE>
			     <FSET ,BOTTLE ,OPENBIT>
			     <FCLEAR ,CYCLOPS ,FIGHTBIT>
			     <TELL
"The cyclops takes the bottle, checks that it's open, and drinks the water.
A moment later, he lets out a yawn that nearly blows you over, and then
falls fast asleep (what did you put in that drink, anyway?)." CR>
			     <SETG CYCLOPS-FLAG T>)
			    (T
			     <TELL
"The cyclops apparently is not thirsty and refuses your generous offer." CR>)>)
		     (<EQUAL? ,PRSO ,GARLIC>
		      <TELL
"The cyclops may be hungry, but there is a limit." CR>)
		     (T
		      <TELL
"The cyclops is not so stupid as to eat THAT!" CR>)>)
	      (<VERB? THROW ATTACK MUNG>
	       <ENABLE <QUEUE I-CYCLOPS -1>>
	       <COND (<VERB? MUNG>
		      <TELL
"\"Do you think I'm as stupid as my father was?\", he says, dodging." CR>)
		     (T
		      <TELL
"The cyclops shrugs but otherwise ignores your pitiful attempt." CR>
		      <COND (<VERB? THROW>
			     <MOVE ,PRSO ,HERE>)>
		      <RTRUE>)>)
	      (<VERB? TAKE>
	       <TELL
"The cyclops doesn't take kindly to being grabbed." CR>)
	      (<VERB? TIE>
	       <TELL
"You cannot tie the cyclops, though he is fit to be tied." CR>)
	      (<VERB? LISTEN>
	       <TELL
"You can hear his stomach rumbling." CR>)>>

<ROUTINE I-CYCLOPS ()
	 <COND (<OR ,CYCLOPS-FLAG ,DEAD> <RTRUE>)
	       (<NOT <EQUAL? ,HERE ,CYCLOPS-ROOM>>
		<DISABLE <INT I-CYCLOPS>>)
	       (T
		<COND (<G? <ABS ,CYCLOWRATH> 5>
		       <DISABLE <INT I-CYCLOPS>>
		       <JIGS-UP
"The cyclops, tired of all of your games and trickery, grabs you firmly.
As he licks his chops, he says \"Mmm. Just like Mom used to make 'em.\"
It's nice to be appreciated.">)
		      (T
		       <COND (<L? ,CYCLOWRATH 0>
			      <SETG CYCLOWRATH <- ,CYCLOWRATH 1>>)
			     (T
			      <SETG CYCLOWRATH <+ ,CYCLOWRATH 1>>)>
		       <COND (<NOT ,CYCLOPS-FLAG>
			      <TELL <NTH ,CYCLOMAD <- <ABS ,CYCLOWRATH> 1>>
				    CR>)>)>)>>

<ROUTINE CYCLOPS-ROOM-FCN (RARG)
	<COND (<EQUAL? .RARG ,M-LOOK>
	       <TELL
"This room has an exit on the northwest, and a staircase leading up." CR>
	       <COND (<AND ,CYCLOPS-FLAG <NOT ,MAGIC-FLAG>>
		      <TELL
"The cyclops is sleeping blissfully at the foot of the stairs." CR>)
		     (,MAGIC-FLAG
		      <TELL
"The east wall, previously solid, now has a cyclops-sized opening in it." CR>)
		     (<0? ,CYCLOWRATH>
		      <TELL
"A cyclops, who looks prepared to eat horses (much less mere
adventurers), blocks the staircase. From his state of health, and
the bloodstains on the walls, you gather that he is not very
friendly, though he likes people." CR>)
		     (<G? ,CYCLOWRATH 0>
		      <TELL
"The cyclops is standing in the corner, eyeing you closely. I don't
think he likes you very much. He looks extremely hungry, even for a
cyclops." CR>)
		     (<L? ,CYCLOWRATH 0>
		      <TELL
"The cyclops, having eaten the hot peppers, appears to be gasping.
His enflamed tongue protrudes from his man-sized mouth." CR>)>)
	      (<EQUAL? .RARG ,M-ENTER>
	       <OR <0? ,CYCLOWRATH> <ENABLE <INT I-CYCLOPS>>>)>>

<GLOBAL CYCLOMAD
	<TABLE (PURE)
	  "The cyclops seems somewhat agitated."
	  "The cyclops appears to be getting more agitated."
	  "The cyclops is moving about the room, looking for something."
	  "The cyclops was looking for salt and pepper. No doubt they are
condiments for his upcoming snack."
	  "The cyclops is moving toward you in an unfriendly manner."
	  "You have two choices: 1. Leave  2. Become dinner.">>

\

"SUBTITLE LOUD LOUD LOUD"

<GLOBAL LOUD-FLAG <>>

<ROUTINE LOUD-ROOM-FCN (RARG "AUX" WRD)
	 <COND (<EQUAL? .RARG ,M-LOOK>
		<TELL
"This is a large room with a ceiling which cannot be detected from
the ground. There is a narrow passage from east to west and a stone
stairway leading upward.">
		<COND (<OR ,LOUD-FLAG
			   <AND <NOT ,GATES-OPEN> ,LOW-TIDE>>
		       <TELL " The room is eerie in its quietness.">)
		      (T
		       <TELL " The room is deafeningly loud with an
undetermined rushing sound. The sound seems to reverberate from all
of the walls, making it difficult even to think.">)>
		<CRLF>)
	       (<AND <EQUAL? .RARG ,M-END> ,GATES-OPEN <NOT ,LOW-TIDE>>
		<TELL
"It is unbearably loud here, with an ear-splitting roar seeming to
come from all around you. There is a pounding in your head which won't
stop. With a tremendous effort, you scramble out of the room." CR CR>
		<GOTO <PICK-ONE ,LOUD-RUNS>>
		<RFALSE>)
	       (<EQUAL? .RARG ,M-ENTER>
		<COND (<OR ,LOUD-FLAG
			   <AND <NOT ,GATES-OPEN> ,LOW-TIDE>>
		       <RFALSE>)
		      (<AND ,GATES-OPEN <NOT ,LOW-TIDE>>
		       <RFALSE>)
		      (T
		       <V-FIRST-LOOK>
		       <COND (,P-CONT
			      <TELL
"The rest of your commands have been lost in the noise." CR>
			      <SETG P-CONT <>>)>
		       <REPEAT ()
			       <COND (<NOT ,SUPER-BRIEF> <CRLF>)>
			       <TELL ">">
			       <READ ,P-INBUF ,P-LEXV>
			       <COND (<0? <GETB ,P-LEXV ,P-LEXWORDS>>
			              <TELL "I beg your pardon?" CR>
				      <AGAIN>)>
			       <SET WRD <GET ,P-LEXV 1>>
			       <COND (<EQUAL? .WRD ,W?GO ,W?WALK ,W?RUN>
				      <SET WRD <GET ,P-LEXV 3>>)
				     (<EQUAL? .WRD ,W?SAY>
				      <SET WRD <GET ,P-LEXV 5>>)>
			       <COND (<EQUAL? .WRD ,W?SAVE>
				      <V-SAVE>)
				     (<EQUAL? .WRD ,W?RESTORE>
				      <V-RESTORE>)
				     (<EQUAL? .WRD ,W?Q ,W?QUIT>
				      <V-QUIT>)
				     (<EQUAL? .WRD ,W?W ,W?WEST>
				      <RETURN <GOTO ,ROUND-ROOM>>)
				     (<EQUAL? .WRD ,W?E ,W?EAST>
				      <RETURN <GOTO ,DAMP-CAVE>>)
				     (<EQUAL? .WRD ,W?U ,W?UP>
				      <RETURN <GOTO ,DEEP-CANYON>>)
				     (<EQUAL? .WRD ,W?BUG>
				      <TELL "That's only your opinion." CR>)
				     (<EQUAL? .WRD ,W?ECHO>
				      <SETG LOUD-FLAG T>
				      <FCLEAR ,BAR ,SACREDBIT>
				      <TELL
"The acoustics of the room change subtly." CR>
				      <COND (<NOT ,SUPER-BRIEF> <CRLF>)>
				      <RETURN>)
				     ;(,DEAD <CRLF>)
				     (T
				      <V-ECHO>)>>)>)>>

<ROUTINE DEEP-CANYON-F (RARG)
	 <COND (<EQUAL? .RARG ,M-LOOK>
		<TELL
"You are on the south edge of a deep canyon. Passages lead off to the
east, northwest and southwest. A stairway leads down.">
		<COND (<AND ,GATES-OPEN <NOT ,LOW-TIDE>>
		       <TELL
" You can hear a loud roaring sound, like that of rushing water, from
below.">)
		      (<AND <NOT ,GATES-OPEN> ,LOW-TIDE>
		       <CRLF>
		       <RTRUE>)
		      (T
		       <TELL
" You can hear the sound of flowing water from below.">)>
		<CRLF>)>>


<GLOBAL EGG-SOLVE <>>

\

"SUBTITLE A SEEDY LOOKING GENTLEMAN..."

<GLOBAL THIEF-HERE <>>

;"I-THIEF moved to DEMONS"

\

"SUBTITLE THINGS THIEF MIGHT DO"

"INTERACTION WITH ADVENTURER -- RETURNS T IF THIEF FINISHED."

<ROUTINE THIEF-VS-ADVENTURER (HERE? "AUX" ROBBED? (WINNER-ROBBED? <>))
  <COND (<AND <NOT ,DEAD> <EQUAL? ,HERE ,TREASURE-ROOM>>)
        (<NOT ,THIEF-HERE>
         <COND (<AND <NOT ,DEAD> <NOT .HERE?> <PROB 30>>
	        <COND (<IN? ,STILETTO ,THIEF>
		       <FCLEAR ,THIEF ,INVISIBLE>
		       <TELL
"Someone carrying a large bag is casually leaning against one of the
walls here. He does not speak, but it is clear from his aspect that
the bag will be taken only over his dead body." CR>
		       <SETG THIEF-HERE T>
		       <RTRUE>)
		      ;(<IN? ,STILETTO ,WINNER>
		       <MOVE ,STILETTO ,THIEF>
		       <FSET ,STILETTO ,NDESCBIT>
		       <FCLEAR ,THIEF ,INVISIBLE>
		       <TELL
"You feel a light finger-touch, and turning, notice a grinning figure
holding a large bag in one hand and a stiletto in the other.">
		       <SETG THIEF-HERE T>
		       <RTRUE>)>)
	       (<AND .HERE?
		     <FSET? ,THIEF ,FIGHTBIT>
		     <NOT <WINNING? ,THIEF>>>
		<TELL
"Your opponent, determining discretion to be the better part of
valor, decides to terminate this little contretemps. With a rueful
nod of his head, he steps backward into the gloom and disappears." CR>
		<FSET ,THIEF ,INVISIBLE>
		<FCLEAR ,THIEF ,FIGHTBIT>
		<RECOVER-STILETTO>
		<RTRUE>)
	       (<AND .HERE? <FSET? ,THIEF ,FIGHTBIT> <PROB 90>>
		<RFALSE>)
	       (<AND .HERE? <PROB 30>>
	        <TELL
"The holder of the large bag just left, looking disgusted.
Fortunately, he took nothing." CR>
		<FSET ,THIEF ,INVISIBLE>
		<RECOVER-STILETTO>
	        <RTRUE>)
	       (<PROB 70> <RFALSE>)
	       (<NOT ,DEAD>
		<COND (<ROB ,HERE ,THIEF 100>
		       <SET ROBBED? ,HERE>)
		      (<ROB ,WINNER ,THIEF>
		       <SET ROBBED? ,PLAYER>)>
		<SETG THIEF-HERE T>
	        <COND (<AND .ROBBED? <NOT .HERE?>>
		       <TELL
"A seedy-looking individual with a large bag just wandered through
the room. On the way through, he quietly abstracted some valuables from ">
		       <COND (<EQUAL? .ROBBED? ,HERE>
			      <TELL "the room">)
			     (ELSE
			      <TELL "your possession">)>
		       <TELL ", mumbling something about
\"Doing unto others before...\"" CR>
		       <STOLE-LIGHT?>)
		      (.HERE?
		       <RECOVER-STILETTO>
		       <COND (.ROBBED?
			      <TELL
"The thief just left, still carrying his large bag. You may
not have noticed that he ">
			      <COND (<EQUAL? .ROBBED? ,PLAYER>
				     <TELL
"robbed you blind first.">)
				    (T
				     <TELL
"appropriated the valuables in the room.">)>
			      <CRLF>
			      <STOLE-LIGHT?>)
			     (T
			      <TELL
"The thief, finding nothing of value, left disgusted." CR>)>
		       <FSET ,THIEF ,INVISIBLE>
		       <SET HERE? <>>
		       <RTRUE>)
		      (T
		       <TELL
"A \"lean and hungry\" gentleman just wandered through, carrying a
large bag. Finding nothing of value, he left disgruntled." CR>
		       <RTRUE>)>)>)
	(T
	 <COND (.HERE?			;"Here, already announced."
		<COND (<PROB 30>
		       <COND (<ROB ,HERE ,THIEF 100>
			      <SET ROBBED? ,HERE>)
			     (<ROB ,WINNER ,THIEF>
			      <SET ROBBED? ,PLAYER>)>
		       <COND (.ROBBED?
			      <TELL
"The thief just left, still carrying his large bag. You may
not have noticed that he ">
			      <COND (<EQUAL? .ROBBED? ,PLAYER>
				     <TELL
"robbed you blind first.">)
				    (T
				     <TELL
"appropriated the valuables in the room.">)>
			      <CRLF>
			      <STOLE-LIGHT?>)
			     (T
			      <TELL
"The thief, finding nothing of value, left disgusted." CR>)>
		       <FSET ,THIEF ,INVISIBLE>
		       <SET HERE? <>>
		       <RECOVER-STILETTO>)>)>)>
       <RFALSE>>

<ROUTINE STOLE-LIGHT? ("AUX" OLD-LIT)
	 <SET OLD-LIT ,LIT>
	 <SETG LIT <LIT? ,HERE>>
	 <COND (<AND <NOT ,LIT> .OLD-LIT>
		<TELL "The thief seems to have left you in the dark." CR>)>
	 <RTRUE>>

"SNARF STILETTO IF DROPPED IT"

;"RECOVER-STILETTO moved to DEMONS"

"PUT HIS BOOTY IN TREASURE ROOM"

<ROUTINE HACK-TREASURES ("AUX" X)
	 <RECOVER-STILETTO>
	 <FSET ,THIEF ,INVISIBLE>
	 <SET X <FIRST? ,TREASURE-ROOM>>
	 <REPEAT ()
		 <COND (<NOT .X> <RETURN>)
		       (T <FCLEAR .X ,INVISIBLE>)>
		 <SET X <NEXT? .X>>>>

<ROUTINE DEPOSIT-BOOTY (RM "AUX" X N (FLG <>))
	 <SET X <FIRST? ,THIEF>>
	 <REPEAT ()
		 <COND (<NOT .X> <RETURN .FLG>)>
		 <SET N <NEXT? .X>>
		 <COND (<EQUAL? .X ,STILETTO ,LARGE-BAG>)
		       (<G? <GETP .X ,P?TVALUE> 0>
			<MOVE .X .RM>
			<SET FLG T>
			<COND (<EQUAL? .X ,EGG>
			       <SETG EGG-SOLVE T>
			       <FSET ,EGG ,OPENBIT>)>)>
		 <SET X .N>>>

"TAKE ALL OF THE VALUABLES SOMEWHERE AND PUT THEM SOMEWHERE ELSE"

"MOVED TO DEMONS"

"ROB MAZE"

<ROUTINE ROB-MAZE (RM "AUX" X N)
	 <SET X <FIRST? .RM>>
	 <REPEAT ()
		 <COND (<NOT .X> <RFALSE>)>
		 <SET N <NEXT? .X>>
		 <COND (<AND <FSET? .X ,TAKEBIT>
			     <NOT <FSET? .X ,INVISIBLE>>
			     <PROB 40>>
			<TELL
"You hear, off in the distance, someone saying \"My, I wonder what
this fine " D .X " is doing here.\"" CR>
			<COND (<PROB 60 80>
			       <MOVE .X ,THIEF>
			       <FSET .X ,TOUCHBIT>
			       <FSET .X ,INVISIBLE>)>
			<RETURN>)>
		 <SET X .N>>>

"STEAL SOME JUNK - moved to DEMONS"

"DROP SOME JUNK - moved to DEMONS"



\

"ROBBER-FUNCTION -- more prosaic thiefly occupations"

<GLOBAL THIEF-ENGROSSED <>>

<ROUTINE ROBBER-FUNCTION ("OPTIONAL" (MODE <>) "AUX" (FLG <>) X N)
	 <COND (<VERB? TELL>
		<TELL "The thief is a strong, silent type." CR>
		<SETG P-CONT <>>)
	       (<NOT .MODE>
		<COND (<AND <VERB? HELLO>
			    <EQUAL? <GETP ,THIEF ,P?LDESC> ,ROBBER-U-DESC>>
		       <TELL
"The thief, being temporarily incapacitated, is unable to acknowledge
your greeting with his usual graciousness." CR>)
		      (<AND <EQUAL? ,PRSO ,KNIFE>
			    <VERB? THROW>
			    <NOT <FSET? ,THIEF ,FIGHTBIT>>>
		       <MOVE ,PRSO ,HERE>
		       <COND (<PROB 10 0>
			      <TELL
"You evidently frightened the robber, though you didn't hit him. He
flees">
			      <REMOVE ,LARGE-BAG>
			      <SET X <>>
			      <COND (<IN? ,STILETTO ,THIEF>
				     <REMOVE ,STILETTO>
				     <SET X T>)>
			      <COND (<FIRST? ,THIEF>
				     <MOVE-ALL ,THIEF ,HERE>
				     <TELL
", but the contents of his bag fall on the floor.">)
				    (T
				     <TELL ".">)>
			      <MOVE ,LARGE-BAG ,THIEF>
			      <COND (.X <MOVE ,STILETTO ,THIEF>)>
			      <CRLF>
			      <FSET ,THIEF ,INVISIBLE>)
			     (T
			      <TELL
"You missed. The thief makes no attempt to take the knife, though it
would be a fine addition to the collection in his bag. He does seem
angered by your attempt." CR>
			      <FSET ,THIEF ,FIGHTBIT>)>)
		      (<AND <VERB? THROW GIVE>
			    ,PRSO
			    <NOT <EQUAL? ,PRSO ,THIEF>>
			    <EQUAL? ,PRSI ,THIEF>>
		       <COND (<L? <GETP ,THIEF ,P?STRENGTH> 0>
			      <PUTP ,THIEF
				    ,P?STRENGTH
				    <- <GETP ,THIEF ,P?STRENGTH>>>
			      <ENABLE <INT I-THIEF>>
			      <RECOVER-STILETTO>
			      <PUTP ,THIEF ,P?LDESC ,ROBBER-C-DESC>
			      <TELL
"Your proposed victim suddenly recovers consciousness." CR>)>
		       <MOVE ,PRSO ,THIEF>
		       <COND ;(<EQUAL? ,PRSO ,STILETTO>
			      <TELL
"The thief takes his stiletto and salutes you with a small nod of
his head." CR>)
			     (<G? <GETP ,PRSO ,P?TVALUE> 0>
			      <SETG THIEF-ENGROSSED T>
			      <TELL
"The thief is taken aback by your unexpected generosity, but accepts
the " D ,PRSO " and stops to admire its beauty." CR>)
			     (T
			      <TELL
"The thief places the " D ,PRSO " in his bag and thanks
you politely." CR>)>)
		      (<VERB? TAKE>
		       <TELL
"Once you got him, what would you do with him?" CR>)
		      (<VERB? EXAMINE LOOK-INSIDE>
		       <TELL
"The thief is a slippery character with beady eyes that flit back
and forth. He carries, along with an unmistakable arrogance, a large bag
over his shoulder and a vicious stiletto, whose blade is aimed
menacingly in your direction. I'd watch out if I were you." CR>)
		      (<VERB? LISTEN>
		       <TELL
"The thief says nothing, as you have not been formally introduced." CR>)>)
	       (<EQUAL? .MODE ,F-BUSY?>
		<COND (<IN? ,STILETTO ,THIEF> <>)
		      (<IN? ,STILETTO <LOC ,THIEF>>
		       <MOVE ,STILETTO ,THIEF>
		       <FSET ,STILETTO ,NDESCBIT>
		       <COND (<IN? ,THIEF ,HERE>
			      <TELL
"The robber, somewhat surprised at this turn of events, nimbly
retrieves his stiletto." CR>)>
		       T)>)
	       (<EQUAL? .MODE ,F-DEAD>
		<MOVE ,STILETTO ,HERE>
		<FCLEAR ,STILETTO ,NDESCBIT>
		<SET X <DEPOSIT-BOOTY ,HERE>>
		<COND (<EQUAL? ,HERE ,TREASURE-ROOM>
		       <SET X <FIRST? ,HERE>>
		       <REPEAT ()
			       <COND
				(<NOT .X>
				 <TELL "The chalice is now safe to take." CR>
				 <RETURN>)
				(<NOT <EQUAL? .X ,CHALICE ,THIEF ,ADVENTURER>>
				 <FCLEAR .X ,INVISIBLE>
				 <COND (<NOT .FLG>
					<SET FLG T>
					<TELL
"As the thief dies, the power of his magic decreases, and his
treasures reappear:" CR>)>
				 <TELL "  A " D .X>
				 <COND (<AND <FIRST? .X>
					     <SEE-INSIDE? .X>>
					<TELL ", with ">
					<PRINT-CONTENTS .X>)>
				 <CRLF>)>
			       <SET X <NEXT? .X>>>)
		      (.X
		       <TELL "His booty remains." CR>)>
		<DISABLE <INT I-THIEF>>)
	       (<EQUAL? .MODE ,F-FIRST?>
		<COND (<AND ,THIEF-HERE
			    <NOT <FSET? ,THIEF ,INVISIBLE>>
			    <PROB 20>>
		       <FSET ,THIEF ,FIGHTBIT>
		       <SETG P-CONT <>>
		       T)>)
	       (<EQUAL? .MODE ,F-UNCONSCIOUS>
		<DISABLE <INT I-THIEF>>
		<FCLEAR ,THIEF ,FIGHTBIT>
		<MOVE ,STILETTO ,HERE>
		<FCLEAR ,STILETTO ,NDESCBIT>
		<PUTP ,THIEF ,P?LDESC ,ROBBER-U-DESC>)
	       (<EQUAL? .MODE ,F-CONSCIOUS>
		<COND (<EQUAL? <LOC ,THIEF> ,HERE>
		       <FSET ,THIEF ,FIGHTBIT>
		       <TELL
"The robber revives, briefly feigning continued unconsciousness, and,
when he sees his moment, scrambles away from you." CR>)>
		<ENABLE <INT I-THIEF>>
		<PUTP ,THIEF ,P?LDESC ,ROBBER-C-DESC>
		<RECOVER-STILETTO>)>>

<GLOBAL ROBBER-C-DESC
"There is a suspicious-looking individual, holding a bag, leaning
against one wall. He is armed with a vicious-looking stiletto.">

<GLOBAL ROBBER-U-DESC
"There is a suspicious-looking individual lying unconscious on the
ground.">

<ROUTINE LARGE-BAG-F ()
	 <COND (<VERB? TAKE>
		<COND (<EQUAL? <GETP ,THIEF ,P?LDESC> ,ROBBER-U-DESC>
		       <TELL
"Sadly for you, the robber collapsed on top of the bag. Trying to take
it would wake him." CR>)
		      (T
		       <TELL
"The bag will be taken over his dead body." CR>)>)
	       (<AND <VERB? PUT>
		     <EQUAL? ,PRSI ,LARGE-BAG>>
		<TELL "It would be a good trick." CR>)
	       (<VERB? OPEN CLOSE>
		<TELL
"Getting close enough would be a good trick." CR>)
	       (<VERB? EXAMINE LOOK-INSIDE>
		<TELL
"The bag is underneath the thief, so one can't say what, if anything, is
inside." CR>)>>

<ROUTINE MOVE-ALL (FROM TO "AUX" X N)
	 <COND (<SET X <FIRST? .FROM>>
		<REPEAT ()
			<COND (<NOT .X> <RETURN>)>
			<SET N <NEXT? .X>>
			<FCLEAR .X ,INVISIBLE>
			<MOVE .X .TO>
			<SET X .N>>)>>

<ROUTINE CHALICE-FCN ()
	 <COND (<VERB? TAKE>
		<COND (<AND <IN? ,PRSO ,TREASURE-ROOM>
			    <IN? ,THIEF ,TREASURE-ROOM>
			    <FSET? ,THIEF ,FIGHTBIT>
			    <NOT <FSET? ,THIEF ,INVISIBLE>>
			    <NOT <EQUAL? <GETP ,THIEF ,P?LDESC>
					 ,ROBBER-U-DESC>>>
		       <TELL
"You'd be stabbed in the back first." CR>)>)
	       (<AND <VERB? PUT> <EQUAL? ,PRSI ,CHALICE>>
		<TELL
"You can't. It's not a very good chalice, is it?" CR>)
	       (T <DUMB-CONTAINER>)>>

<ROUTINE TREASURE-ROOM-FCN (RARG "AUX" TL)
	 <COND (<AND <EQUAL? .RARG ,M-ENTER>
		     <1? <GET <INT I-THIEF> ,C-ENABLED?>>
		     <NOT ,DEAD>>
		<COND (<NOT <IN? ,THIEF ,HERE>>
		       <TELL
"You hear a scream of anguish as you violate the robber's hideaway.
Using passages unknown to you, he rushes to its defense." CR>
		       <MOVE ,THIEF ,HERE>)>
		<FSET ,THIEF ,FIGHTBIT>
		<FCLEAR ,THIEF ,INVISIBLE>
		<THIEF-IN-TREASURE>)>>

<ROUTINE THIEF-IN-TREASURE ("AUX" F N)
	 <SET F <FIRST? ,HERE>>
	 <COND (<AND .F <NEXT? .F>>
		<TELL
"The thief gestures mysteriously, and the treasures in the room
suddenly vanish." CR CR>)>
	 <REPEAT ()
		 <COND (<NOT .F> <RETURN>)
		       (<NOT <EQUAL? .F ,CHALICE ,THIEF>>
			<FSET .F ,INVISIBLE>)>
		 <SET F <NEXT? .F>>>>

<ROUTINE FRONT-DOOR-FCN ()
	 <COND (<VERB? OPEN>
		<TELL "The door cannot be opened." CR>)
	       (<VERB? BURN>
		<TELL
		 "You cannot burn this door." CR>)
	       (<VERB? MUNG>
		<TELL "You can't seem to damage the door." CR>)
	       (<VERB? LOOK-BEHIND>
		<TELL "It won't open." CR>)>>

\

"SUBTITLE RANDOM FUNCTIONS"

<ROUTINE BODY-FUNCTION ()
	 <COND (<VERB? TAKE>
		<TELL "A force keeps you from taking the bodies." CR>)
	       (<VERB? MUNG BURN>
		<JIGS-UP
"The voice of the guardian of the dungeon booms out from the darkness,
\"Your disrespect costs you your life!\" and places your head on a sharp
pole.">)>>

<ROUTINE BLACK-BOOK ()
	 <COND (<VERB? OPEN>
		<TELL "The book is already open to page 569." CR>)
	       (<VERB? CLOSE>
		<TELL "As hard as you try, the book cannot be closed." CR>)
	       (<OR <VERB? TURN>
		    <AND <VERB? READ-PAGE>
			 <EQUAL? ,PRSI ,INTNUM>
			 <NOT <EQUAL? ,P-NUMBER 569>>>>
		<TELL
"Beside page 569, there is only one other page with any legible printing on
it. Most of it is unreadable, but the subject seems to be the banishment of
evil. Apparently, certain noises, lights, and prayers are efficacious in this
regard." CR>)
	       (<VERB? BURN>
		<REMOVE-CAREFULLY ,PRSO>
		<JIGS-UP
"A booming voice says \"Wrong, cretin!\" and you notice that you have
turned into a pile of dust. How, I can't imagine.">)>>

<ROUTINE PAINTING-FCN ()
	 <COND (<VERB? MUNG>
		<PUTP ,PRSO ,P?TVALUE 0>
		<PUTP ,PRSO ,P?LDESC
"There is a worthless piece of canvas here.">
		<TELL
"Congratulations! Unlike the other vandals, who merely stole the
artist's masterpieces, you have destroyed one." CR>)>>

\

"SUBTITLE LET THERE BE LIGHT SOURCES"

<GLOBAL LAMP-TABLE
	<TABLE (PURE)
	       100
	       "The lamp appears a bit dimmer."
	       70
	       "The lamp is definitely dimmer now."
	       15
	       "The lamp is nearly out."
	       0>>

<ROUTINE LANTERN ()
	 <COND (<VERB? THROW>
		<TELL
"The lamp has smashed into the floor, and the light has gone out." CR>
		<DISABLE <INT I-LANTERN>>
		<REMOVE-CAREFULLY ,LAMP>
		<MOVE ,BROKEN-LAMP ,HERE>)
	       (<VERB? LAMP-ON>
		<COND (<FSET? ,LAMP ,RMUNGBIT>
		       <TELL "A burned-out lamp won't light." CR>)
		      (T
		       <ENABLE <INT I-LANTERN>>
		       <>)>)
	       (<VERB? LAMP-OFF>
		<COND (<FSET? ,LAMP ,RMUNGBIT>
		       <TELL "The lamp has already burned out." CR>)
		      (T
		       <DISABLE <INT I-LANTERN>>
		       <>)>)
	       (<VERB? EXAMINE>
		<TELL "The lamp ">
		<COND (<FSET? ,LAMP ,RMUNGBIT>
		       <TELL "has burned out.">)
		      (<FSET? ,LAMP ,ONBIT>
		       <TELL "is on.">)
		      (T
		       <TELL "is turned off.">)>
		<CRLF>)>>

<ROUTINE MAILBOX-F ()
	 <COND (<AND <VERB? TAKE> <EQUAL? ,PRSO ,MAILBOX>>
		<TELL "It is securely anchored." CR>)>>

<GLOBAL MATCH-COUNT 6>

<ROUTINE MATCH-FUNCTION ("AUX" CNT)
	 <COND (<AND <VERB? LAMP-ON BURN> <EQUAL? ,PRSO ,MATCH>>
		<COND (<G? ,MATCH-COUNT 0>
		       <SETG MATCH-COUNT <- ,MATCH-COUNT 1>>)>
		<COND (<NOT <G? ,MATCH-COUNT 0>>
		       <TELL
			"I'm afraid that you have run out of matches." CR>)
		      (<EQUAL? ,HERE ,LOWER-SHAFT ,TIMBER-ROOM>
		       <TELL
"This room is drafty, and the match goes out instantly." CR>)
		      (T
		       <FSET ,MATCH ,FLAMEBIT>
		       <FSET ,MATCH ,ONBIT>
		       <ENABLE <QUEUE I-MATCH 2>>
		       <TELL "One of the matches starts to burn." CR>
		       <COND (<NOT ,LIT>
			      <SETG LIT T>
			      <V-LOOK>)>
		       <RTRUE>)>)
	       (<AND <VERB? LAMP-OFF> <FSET? ,MATCH ,FLAMEBIT>>
		<TELL "The match is out." CR>
		<FCLEAR ,MATCH ,FLAMEBIT>
		<FCLEAR ,MATCH ,ONBIT>
		<SETG LIT <LIT? ,HERE>>
		<COND (<NOT ,LIT> <TELL "It's pitch black in here!" CR>)>
		<QUEUE I-MATCH 0>
		<RTRUE>)
	       (<VERB? COUNT OPEN>
		<TELL "You have ">
	        <SET CNT <- ,MATCH-COUNT 1>>
		<COND (<NOT <G? .CNT 0>> <TELL "no">)
		      (T <TELL N .CNT>)>
		<TELL " match">
		<COND (<NOT <1? .CNT>> <TELL "es.">) (T <TELL ".">)>
		<CRLF>)
	       (<VERB? EXAMINE>
		<COND (<FSET? ,MATCH ,ONBIT>
		       <TELL "The match is burning.">)
		      (T
		       <TELL
"The matchbook isn't very interesting, except for what's written on it.">)>
		<CRLF>)>>

<ROUTINE I-MATCH ()
	 <TELL "The match has gone out." CR>
	 <FCLEAR ,MATCH ,FLAMEBIT>
	 <FCLEAR ,MATCH ,ONBIT>
	 <SETG LIT <LIT? ,HERE>>
	 <RTRUE>>

<ROUTINE I-LANTERN ("AUX" TICK (TBL <VALUE LAMP-TABLE>))
	 <ENABLE <QUEUE I-LANTERN <SET TICK <GET .TBL 0>>>>
	 <LIGHT-INT ,LAMP .TBL .TICK>
	 <COND (<NOT <0? .TICK>>
		<SETG LAMP-TABLE <REST .TBL 4>>)>>

<ROUTINE I-CANDLES ("AUX" TICK (TBL <VALUE CANDLE-TABLE>))
	 <FSET ,CANDLES ,TOUCHBIT>
	 <ENABLE <QUEUE I-CANDLES <SET TICK <GET .TBL 0>>>>
	 <LIGHT-INT ,CANDLES .TBL .TICK>
	 <COND (<NOT <0? .TICK>>
		<SETG CANDLE-TABLE <REST .TBL 4>>)>>

<ROUTINE LIGHT-INT (OBJ TBL TICK)
	 <COND (<0? .TICK>
		<FCLEAR .OBJ ,ONBIT>
		<FSET .OBJ ,RMUNGBIT>)>
	 <COND (<OR <HELD? .OBJ> <IN? .OBJ ,HERE>>
		<COND (<0? .TICK>
		       <TELL
"You'd better have more light than from the " D .OBJ "." CR>)
		      (T
		       <TELL <GET .TBL 1> CR>)>)>>

<ROUTINE MIN (N1 N2)
	 <COND (<L? .N1 .N2> .N1)
	       (T .N2)>>

<ROUTINE CANDLES-FCN ()
	 <COND (<NOT <FSET? ,CANDLES ,TOUCHBIT>>
		<ENABLE <INT I-CANDLES>>)>
	 <COND (<EQUAL? ,CANDLES ,PRSI> <RFALSE>)
	       (T
		<COND (<VERB? LAMP-ON BURN>
		       <COND (<FSET? ,CANDLES ,RMUNGBIT>
			      <TELL
"Alas, there's not much left of the candles. Certainly not enough to
burn." CR>)
			     (<NOT ,PRSI>
			      <COND (<FSET? ,MATCH ,FLAMEBIT>
				     <TELL "(with the match)" CR>
				     <PERFORM ,V?LAMP-ON ,CANDLES ,MATCH>
				     <RTRUE>)
				    (T
				     <TELL
"You should say what to light them with." CR>
				     <RFATAL>)>)
			     (<AND <EQUAL? ,PRSI ,MATCH>
				   <FSET? ,MATCH ,ONBIT>>
			      <TELL "The candles are ">
			      <COND (<FSET? ,CANDLES ,ONBIT>
				     <TELL "already lit." CR>)
				    (T
				     <FSET ,CANDLES ,ONBIT>
				     <TELL "lit." CR>
				     <ENABLE <INT I-CANDLES>>)>)
			     (<EQUAL? ,PRSI ,TORCH>
			      <COND (<FSET? ,CANDLES ,ONBIT>
				     <TELL
"You realize, just in time, that the candles are already lighted." CR>)
				    (T
				     <TELL
"The heat from the torch is so intense that the candles are vaporized." CR>
				     <REMOVE-CAREFULLY ,CANDLES>)>)
			     (T
			      <TELL
"You have to light them with something that's burning, you know." CR>)>)
		      (<VERB? COUNT>
		       <TELL
"Let's see, how many objects in a pair? Don't tell me, I'll get it." CR>)
		      (<VERB? LAMP-OFF>
		       <DISABLE <INT I-CANDLES>>
		       <COND (<FSET? ,CANDLES ,ONBIT>
			      <TELL "The flame is extinguished.">
			      <FCLEAR ,CANDLES ,ONBIT>
			      <FSET ,CANDLES ,TOUCHBIT>
			      <SETG LIT <LIT? ,HERE>>
			      <COND (<NOT ,LIT>
				     <TELL " It's really dark in here....">)>
			      <CRLF>
			      <RTRUE>)
			     (T <TELL "The candles are not lighted." CR>)>)
		      (<AND <VERB? PUT> <FSET? ,PRSI ,BURNBIT>>
		       <TELL "That wouldn't be smart." CR>)
		      (<VERB? EXAMINE>
		       <TELL "The candles are ">
		       <COND (<FSET? ,CANDLES ,ONBIT>
			      <TELL "burning.">)
			     (T <TELL "out.">)>
		       <CRLF>)>)>>

<GLOBAL CANDLE-TABLE
	<TABLE (PURE)
	       20
	       "The candles grow shorter."
	       10
	       "The candles are becoming quite short."
	       5
	       "The candles won't last long now."
	       0>>

<ROUTINE CAVE2-ROOM (RARG)
  <COND (<EQUAL? .RARG ,M-END>
	 <COND (<AND <IN? ,CANDLES ,WINNER>
		     <PROB 50 80>
		     <FSET? ,CANDLES ,ONBIT>>
		<DISABLE <INT I-CANDLES>>
		<FCLEAR ,CANDLES ,ONBIT>
		<TELL
"A gust of wind blows out your candles!" CR>
		<COND (<NOT <SETG LIT <LIT? ,HERE>>>
		       <TELL "It is now completely dark." CR>)>)>)>>

\

"SUBTITLE ASSORTED WEAPONS"

<ROUTINE SWORD-FCN ("AUX" G)
	 <COND (<AND <VERB? TAKE> <EQUAL? ,WINNER ,ADVENTURER>>
		<ENABLE <QUEUE I-SWORD -1>>
		<>)
	       (<VERB? EXAMINE>
		<COND (<EQUAL? <SET G <GETP ,SWORD ,P?TVALUE>> 1>
		       <TELL
"Your sword is glowing with a faint blue glow." CR>)
		      (<EQUAL? .G 2>
		       <TELL
"Your sword is glowing very brightly." CR>)>)>>

"SUBTITLE COAL MINE"

<ROUTINE BOOM-ROOM (RARG "AUX" (DUMMY? <>) FLAME)
         <COND (<EQUAL? .RARG ,M-END>
		<COND (<AND <EQUAL? .RARG ,M-END>
			    <VERB? LAMP-ON BURN>
			    <EQUAL? ,PRSO ,CANDLES ,TORCH ,MATCH>>
		       <SET DUMMY? T>)>
		<COND (<OR <AND <HELD? ,CANDLES>
				<FSET? ,CANDLES ,ONBIT>>
			   <AND <HELD? ,TORCH>
				<FSET? ,TORCH ,ONBIT>>
			   <AND <HELD? ,MATCH>
				<FSET? ,MATCH ,ONBIT>>>
		       <COND (.DUMMY?
			      <TELL
"How sad for an aspiring adventurer to light a " D ,PRSO " in a room which
reeks of gas. Fortunately, there is justice in the world." CR>)
			     (T
			      <TELL
"Oh dear. It appears that the smell coming from this room was coal gas.
I would have thought twice about carrying flaming objects in here." CR>)>
		       <JIGS-UP "|
      ** BOOOOOOOOOOOM **">)>)>>

<ROUTINE BAT-D ("OPTIONAL" FOO)
	 <COND (<EQUAL? <LOC ,GARLIC> ,WINNER ,HERE>
		<TELL
"In the corner of the room on the ceiling is a large vampire bat who
is obviously deranged and holding his nose." CR>)
	       (T
		<TELL
"A large vampire bat, hanging from the ceiling, swoops down at you!" CR>)>>

<ROUTINE BATS-ROOM (RARG)
	 <COND (<EQUAL? .RARG ,M-LOOK>
		<TELL
"You are in a small room which has doors only to the east and south." CR>)
	       (<AND <EQUAL? .RARG ,M-ENTER> <NOT ,DEAD>>
		<COND (<NOT <EQUAL? <LOC ,GARLIC> ,WINNER ,HERE>>
		       <V-LOOK>
		       <CRLF>
		       <FLY-ME>)>)>>

<ROUTINE MACHINE-ROOM-FCN (RARG)
	 <COND (<EQUAL? .RARG ,M-LOOK>
		<TELL
"This is a large, cold room whose sole exit is to the north. In one
corner there is a machine which is reminiscent of a clothes
dryer. On its face is a switch which is labelled \"START\".
The switch does not appear to be manipulable by any human hand (unless the
fingers are about 1/16 by 1/4 inch). On the front of the machine is a large
lid, which is ">
		<COND (<FSET? ,MACHINE ,OPENBIT>
		       <TELL "open.">)
		      (T <TELL "closed.">)>
		<CRLF>)>>

<ROUTINE MACHINE-F ()
	 <COND (<AND <VERB? TAKE> <EQUAL? ,PRSO ,MACHINE>>
		<TELL "It is far too large to carry." CR>)
	       (<VERB? OPEN>
	        <COND (<FSET? ,MACHINE ,OPENBIT>
		       <TELL <PICK-ONE ,DUMMY> CR>)
		      (<FIRST? ,MACHINE>
		       <TELL "The lid opens, revealing ">
		       <PRINT-CONTENTS ,MACHINE>
		       <TELL "." CR>
		       <FSET ,MACHINE ,OPENBIT>)
		      (T
		       <TELL "The lid opens." CR>
		       <FSET ,MACHINE ,OPENBIT>)>)
	       (<VERB? CLOSE>
	        <COND (<FSET? ,MACHINE ,OPENBIT>
		       <TELL "The lid closes." CR>
		       <FCLEAR ,MACHINE ,OPENBIT>
		       T)
		      (T
		       <TELL <PICK-ONE ,DUMMY> CR>)>)
	       (<VERB? LAMP-ON>
		<COND (<NOT ,PRSI>
		       <TELL
"It's not clear how to turn it on with your bare hands." CR>)
		      (T
		       <PERFORM ,V?TURN ,MACHINE-SWITCH ,PRSI>
		       <RTRUE>)>)>>

<ROUTINE MSWITCH-FUNCTION ("AUX" O)
	 <COND (<VERB? TURN>
		<COND (<EQUAL? ,PRSI ,SCREWDRIVER>
		       <COND (<FSET? ,MACHINE ,OPENBIT>
			      <TELL
"The machine doesn't seem to want to do anything." CR>)
			     (T <TELL
"The machine comes to life (figuratively) with a dazzling display of
colored lights and bizarre noises. After a few moments, the
excitement abates." CR>
			      <COND (<IN? ,COAL ,MACHINE>
				     <REMOVE-CAREFULLY ,COAL>
				     <MOVE ,DIAMOND ,MACHINE>)
				    (T
				     <REPEAT ()
					     <COND (<SET O <FIRST? ,MACHINE>>
						    <REMOVE-CAREFULLY .O>)
						   (T <RETURN>)>>
				     <MOVE ,GUNK ,MACHINE>)>)>)
		      (T
		       <TELL "It seems that a " D ,PRSI " won't do." CR>)>)>>

<ROUTINE GUNK-FUNCTION ()
	 <REMOVE-CAREFULLY ,GUNK>
	 <TELL
"The slag was rather insubstantial, and crumbles into dust at your touch." CR>>

<ROUTINE NO-OBJS (RARG "AUX" F)
	 <COND (<EQUAL? .RARG ,M-BEG>
		<SET F <FIRST? ,WINNER>>
		<SETG EMPTY-HANDED T>
		<REPEAT ()
			<COND (<NOT .F> <RETURN>)
			      (<G? <WEIGHT .F> 4>
			       <SETG EMPTY-HANDED <>>
			       <RETURN>)>
			<SET F <NEXT? .F>>>
		<COND (<AND <EQUAL? ,HERE ,LOWER-SHAFT> ,LIT>
		       <SCORE-UPD ,LIGHT-SHAFT>
		       <SETG LIGHT-SHAFT 0>)>
		<RFALSE>)>>

<ROUTINE SOUTH-TEMPLE-FCN (RARG)
	 <COND (<EQUAL? .RARG ,M-BEG>
		<SETG COFFIN-CURE <NOT <IN? ,COFFIN ,WINNER>>>
		<RFALSE>)>>

<GLOBAL LIGHT-SHAFT 13>
<GDECL (LIGHT-SHAFT) FIX>

\

"SUBTITLE OLD MAN RIVER, THAT OLD MAN RIVER..."

<ROUTINE WHITE-CLIFFS-FUNCTION (RARG)
	 <COND (<EQUAL? .RARG ,M-END>
		<COND (<IN? ,INFLATED-BOAT ,WINNER>
		       <SETG DEFLATE <>>)
	       	      (T
		       <SETG DEFLATE T>)>)>>

<ROUTINE SCEPTRE-FUNCTION ()
	 <COND (<VERB? WAVE RAISE>
		<COND (<OR <EQUAL? ,HERE ,ARAGAIN-FALLS>
			   <EQUAL? ,HERE ,END-OF-RAINBOW>>
		       <COND (<NOT ,RAINBOW-FLAG>
			      <FCLEAR ,POT-OF-GOLD ,INVISIBLE>
			      <TELL
"Suddenly, the rainbow appears to become solid and, I venture,
walkable (I think the giveaway was the stairs and bannister)." CR>
			      <COND (<AND <EQUAL? ,HERE ,END-OF-RAINBOW>
					  <IN? ,POT-OF-GOLD ,END-OF-RAINBOW>>
				     <TELL
"A shimmering pot of gold appears at the end of the rainbow." CR>)>
			      <SETG RAINBOW-FLAG T>)
			     (T
			      <ROB ,ON-RAINBOW ,WALL>
			      <TELL
"The rainbow seems to have become somewhat run-of-the-mill." CR>
			      <SETG RAINBOW-FLAG <>>
			      <RTRUE>)>)
		      (<EQUAL? ,HERE ,ON-RAINBOW>
		       <SETG RAINBOW-FLAG <>>
		       <JIGS-UP
"The structural integrity of the rainbow is severely compromised,
leaving you hanging in midair, supported only by water vapor. Bye.">)
		      (T
		       <TELL
"A dazzling display of color briefly emanates from the sceptre." CR>)>)>>

<ROUTINE FALLS-ROOM (RARG)
    <COND (<EQUAL? .RARG ,M-LOOK>
	   <TELL
"You are at the top of Aragain Falls, an enormous waterfall with a
drop of about 450 feet. The only path here is on the north end." CR>
	   <COND (,RAINBOW-FLAG
		  <TELL
"A solid rainbow spans the falls.">)
		 (T
		  <TELL
"A beautiful rainbow can be seen over the falls and to the west.">)>
	   <CRLF>)>>

<ROUTINE RAINBOW-FCN ()
	 <COND (<VERB? CROSS THROUGH>
		<COND (<EQUAL? ,HERE ,CANYON-VIEW>
		       <TELL "From here?!?" CR>
		       <RTRUE>)>
		<COND (,RAINBOW-FLAG
		       <COND (<EQUAL? ,HERE ,ARAGAIN-FALLS>
			      <GOTO ,END-OF-RAINBOW>)
			     (<EQUAL? ,HERE ,END-OF-RAINBOW>
			      <GOTO ,ARAGAIN-FALLS>)
			     (T
			      <TELL "You'll have to say which way..." CR>)>)
		      (T
		       <TELL "Can you walk on water vapor?"
			     CR>)>)
	       (<VERB? LOOK-UNDER>
		<TELL "The Frigid River flows under the rainbow." CR>)>>

<ROUTINE DBOAT-FUNCTION ("AUX")
	 <COND (<AND <VERB? PUT PUT-ON>
		     <EQUAL? ,PRSO ,PUTTY>>
		<FIX-BOAT>)
	       (<VERB? INFLATE FILL>
		<TELL
"No chance. Some moron punctured it." CR>)
	       (<VERB? PLUG>
		<COND (<EQUAL? ,PRSI ,PUTTY>
		       <FIX-BOAT>)
		      (T <WITH-TELL ,PRSI>)>)>>

<ROUTINE FIX-BOAT ()
	 <TELL "Well done. The boat is repaired." CR>
	 <MOVE ,INFLATABLE-BOAT <LOC ,PUNCTURED-BOAT>>
	 <REMOVE-CAREFULLY ,PUNCTURED-BOAT>>

<ROUTINE RIVER-FUNCTION ()
	 <COND (<VERB? PUT>
		<COND (<EQUAL? ,PRSI ,RIVER>
		       <COND (<EQUAL? ,PRSO ,ME>
			      <JIGS-UP
"You splash around for a while, fighting the current, then you drown.">)
			     (<EQUAL? ,PRSO ,INFLATED-BOAT>
			      <TELL
"You should get in the boat then launch it." CR>)
			     (<FSET? ,PRSO ,BURNBIT>
			      <REMOVE-CAREFULLY ,PRSO>
			      <TELL
"The " D ,PRSO " floats for a moment, then sinks." CR>)
			     (T
			      <REMOVE-CAREFULLY ,PRSO>
			      <TELL
"The " D ,PRSO " splashes into the water and is gone forever." CR>)>)>)
	       (<VERB? LEAP THROUGH>
		<TELL
"A look before leaping reveals that the river is wide and dangerous,
with swift currents and large, half-hidden rocks. You decide to forgo your
swim." CR>)>>

<GLOBAL RIVER-SPEEDS
	<LTABLE (PURE) RIVER-1 4 RIVER-2 4 RIVER-3 3 RIVER-4 2 RIVER-5 1>>

<GLOBAL RIVER-NEXT
	<LTABLE (PURE) RIVER-1 RIVER-2 RIVER-3 RIVER-4 RIVER-5>>

<GLOBAL RIVER-LAUNCH
	<LTABLE (PURE) DAM-BASE RIVER-1
		WHITE-CLIFFS-NORTH RIVER-3
		WHITE-CLIFFS-SOUTH RIVER-4
		SHORE RIVER-5
		SANDY-BEACH RIVER-4
		RESERVOIR-SOUTH RESERVOIR
		RESERVOIR-NORTH RESERVOIR
		STREAM-VIEW IN-STREAM>>

<ROUTINE I-RIVER ("AUX" RM)
	 <COND (<AND <NOT <EQUAL? ,HERE ,RIVER-1 ,RIVER-2 ,RIVER-3>>
		     <NOT <EQUAL? ,HERE ,RIVER-4 ,RIVER-5>>>
		<DISABLE <INT I-RIVER>>)
	       (<SET RM <LKP ,HERE ,RIVER-NEXT>>
		<TELL "The flow of the river carries you downstream." CR CR>
		<GOTO .RM>
		<ENABLE <QUEUE I-RIVER <LKP ,HERE ,RIVER-SPEEDS>>>)
	       (T
		<JIGS-UP
"Unfortunately, the magic boat doesn't provide protection from
the rocks and boulders one meets at the bottom of waterfalls.
Including this one.">)>>

<ROUTINE RBOAT-FUNCTION ("OPTIONAL" (RARG <>) "AUX" TMP)
    <COND (<EQUAL? .RARG ,M-ENTER ,M-END ,M-LOOK> <>)
	  (<EQUAL? .RARG ,M-BEG>
	   <COND (<VERB? WALK>
		  <COND (<EQUAL? ,PRSO ,P?LAND ,P?EAST ,P?WEST>
			 <RFALSE>)
			(<AND <EQUAL? ,HERE ,RESERVOIR>
			      <EQUAL? ,PRSO ,P?NORTH ,P?SOUTH>>
			 <RFALSE>)
			(<AND <EQUAL? ,HERE ,IN-STREAM>
			      <EQUAL? ,PRSO ,P?SOUTH>>
			 <RFALSE>)
			(T
			 <TELL
"Read the label for the boat's instructions." CR>
			 <RTRUE>)>)
		 (<VERB? LAUNCH>
		  <COND (<OR <EQUAL? ,HERE ,RIVER-1 ,RIVER-2 ,RIVER-3>
			     <EQUAL? ,HERE ,RIVER-4 ,RESERVOIR ,IN-STREAM>>
			 <TELL
"You are on the ">
			 <COND (<EQUAL? ,HERE ,RESERVOIR>
				<TELL "reservoir">)
			       (<EQUAL? ,HERE ,IN-STREAM>
				<TELL "stream">)
			       (T <TELL "river">)>
			 <TELL ", or have you forgotten?" CR>)
			(<EQUAL? <SET TMP <GO-NEXT ,RIVER-LAUNCH>> 1>
			 <ENABLE <QUEUE I-RIVER <LKP ,HERE ,RIVER-SPEEDS>>>
			 <RTRUE>)
			(<NOT <EQUAL? .TMP 2>>
			 <TELL "You can't launch it here." CR>
			 <RTRUE>)
			(T <RTRUE>)>)
		 (<OR <AND <VERB? DROP>
			   <FSET? ,PRSO ,WEAPONBIT>>
		      <AND <VERB? PUT>
			   <FSET? ,PRSO ,WEAPONBIT>
			   <EQUAL? ,PRSI ,INFLATED-BOAT>>
		      <AND <VERB? ATTACK MUNG>
			   <FSET? ,PRSI ,WEAPONBIT>>>
		  <REMOVE-CAREFULLY ,INFLATED-BOAT>
		  <MOVE ,PUNCTURED-BOAT ,HERE>
		  <ROB ,INFLATED-BOAT ,HERE>
		  <MOVE ,WINNER ,HERE>
		  <TELL
"It seems that the ">
		  <COND (<VERB? DROP PUT> <TELL D ,PRSO>)
			(T <TELL D ,PRSI>)>
		  <TELL " didn't agree with the boat, as evidenced
by the loud hissing noise issuing therefrom. With a pathetic sputter, the
boat deflates, leaving you without." CR>
		  <COND (<FSET? ,HERE ,NONLANDBIT>
			 <CRLF>
			 <COND (<==? ,HERE ,RESERVOIR ,IN-STREAM>
				<JIGS-UP
"Another pathetic sputter, this time from you, heralds your drowning.">)
			       (T
				<JIGS-UP
"In other words, fighting the fierce currents of the Frigid River. You
manage to hold your own for a bit, but then you are carried over a
waterfall and into some nasty rocks. Ouch!">)>)>
		  <RTRUE>)
		 (<VERB? LAUNCH>
	  	   <TELL "You're not in the boat!" CR>)>)
	  (<VERB? BOARD>
	   <COND (<OR <IN? ,SCEPTRE ,WINNER>
		      <IN? ,KNIFE ,WINNER>
		      <IN? ,SWORD ,WINNER>
		      <IN? ,RUSTY-KNIFE ,WINNER>
		      <IN? ,AXE ,WINNER>
		      <IN? ,STILETTO ,WINNER>>
		  <TELL
"Oops! Something sharp seems to have slipped and punctured the boat.
The boat deflates to the sounds of hissing, sputtering, and cursing." CR>
		  <REMOVE-CAREFULLY ,INFLATED-BOAT>
		  <MOVE ,PUNCTURED-BOAT ,HERE>
		  <THIS-IS-IT ,PUNCTURED-BOAT>
		  T)>)
	  (<VERB? INFLATE FILL>
	   <TELL "Inflating it further would probably burst it." CR>)
	  (<VERB? DEFLATE>
	   <COND (<EQUAL? <LOC ,WINNER> ,INFLATED-BOAT>
		  <TELL
"You can't deflate the boat while you're in it." CR>)
		 (<NOT <IN? ,INFLATED-BOAT ,HERE>>
		  <TELL
"The boat must be on the ground to be deflated." CR>)
		 (T <TELL
"The boat deflates." CR>
		  <SETG DEFLATE T>
		  <REMOVE-CAREFULLY ,INFLATED-BOAT>
		  <MOVE ,INFLATABLE-BOAT ,HERE>
		  <THIS-IS-IT ,INFLATABLE-BOAT>)>)>>

<ROUTINE BREATHE ()
	 <PERFORM ,V?INFLATE ,PRSO ,LUNGS>>

<ROUTINE IBOAT-FUNCTION ()
	 <COND (<VERB? INFLATE FILL>
		<COND (<NOT <IN? ,INFLATABLE-BOAT ,HERE>>
		       <TELL
"The boat must be on the ground to be inflated." CR>)
		      (<EQUAL? ,PRSI ,PUMP>
		       <TELL
"The boat inflates and appears seaworthy." CR>
		       <COND (<NOT <FSET? ,BOAT-LABEL ,TOUCHBIT>>
			      <TELL
"A tan label is lying inside the boat." CR>)>
		       <SETG DEFLATE <>>
		       <REMOVE-CAREFULLY ,INFLATABLE-BOAT>
		       <MOVE ,INFLATED-BOAT ,HERE>
		       <THIS-IS-IT ,INFLATED-BOAT>)
		      (<EQUAL? ,PRSI ,LUNGS>
		       <TELL
"You don't have enough lung power to inflate it." CR>)
		      (T
		       <TELL
"With a " D ,PRSI "? Surely you jest!" CR>)>)>>

<GLOBAL BUOY-FLAG T>

<ROUTINE RIVR4-ROOM (RARG)
	 <COND (<EQUAL? .RARG ,M-END>
		<COND (<AND <IN? ,BUOY ,WINNER> ,BUOY-FLAG>
	      	       <TELL
"You notice something funny about the feel of the buoy." CR>
		       <SETG BUOY-FLAG <>>)>)>>

<GLOBAL BEACH-DIG -1>

<GDECL (BEACH-DIG) FIX>

<ROUTINE SAND-FUNCTION ()
	 <COND (<AND <VERB? DIG> <==? ,PRSI ,SHOVEL>>
		<SETG BEACH-DIG <+ 1 ,BEACH-DIG>>
		<COND (<G? ,BEACH-DIG 3>
		       <SETG BEACH-DIG -1>
		       <AND <IN? ,SCARAB ,HERE> <FSET ,SCARAB ,INVISIBLE>>
		       <JIGS-UP "The hole collapses, smothering you.">)
		      (<EQUAL? ,BEACH-DIG 3>
		       <COND (<FSET? ,SCARAB ,INVISIBLE>
			      <TELL
"You can see a scarab here in the sand." CR>
			      <THIS-IS-IT ,SCARAB>
			      <FCLEAR ,SCARAB ,INVISIBLE>)>)
		      (T
		       <TELL <GET ,BDIGS ,BEACH-DIG> CR>)>)>>

<GLOBAL BDIGS
	<TABLE (PURE) "You seem to be digging a hole here."
	       "The hole is getting deeper, but that's about it."
	       "You are surrounded by a wall of sand on all sides.">>

\

"SUBTITLE TOITY POIPLE BOIDS A CHOIPIN' AN' A BOIPIN' ... "

<ROUTINE TREE-ROOM (RARG "AUX" F)
	 <COND (<EQUAL? .RARG ,M-LOOK>
		<TELL
"You are about 10 feet above the ground nestled among some large
branches. The nearest branch above you is above your reach." CR>
		<COND (<AND <SET F <FIRST? ,PATH>>
			    <NEXT? .F>>
		       <TELL "On the ground below you can see:  ">
		       <PRINT-CONTENTS ,PATH>
		       <TELL "." CR>)>)
	       (<EQUAL? .RARG ,M-BEG>
		<COND (<AND <VERB? CLIMB-DOWN> <EQUAL? ,PRSO ,TREE ,ROOMS>>
		       <DO-WALK ,P?DOWN>)
		      (<AND <VERB? CLIMB-UP CLIMB-FOO>
			    <EQUAL? ,PRSO ,TREE>>
		       <DO-WALK ,P?UP>)
		      (<VERB? DROP>
		       <COND (<NOT <IDROP>> <RTRUE>)
			     (<AND <EQUAL? ,PRSO ,NEST> <IN? ,EGG ,NEST>>
			      <TELL
"The nest falls to the ground, and the egg spills out of it, seriously
damaged." CR>
			      <REMOVE-CAREFULLY ,EGG>
			      <MOVE ,BROKEN-EGG ,PATH>)
			     (<EQUAL? ,PRSO ,EGG>
			      <TELL
"The egg falls to the ground and springs open, seriously damaged.">
			      <MOVE ,EGG ,PATH>
			      <BAD-EGG>
			      <CRLF>)
			     (<NOT <EQUAL? ,PRSO ,WINNER ,TREE>>
			      <MOVE ,PRSO ,PATH>
			      <TELL
"The " D ,PRSO " falls to the ground." CR>)
			     (<VERB? LEAP>
			      <JIGS-UP
			        "That was just a bit too far down.">)>)>)
	       (<EQUAL? .RARG ,M-ENTER> <ENABLE <QUEUE I-FOREST-ROOM -1>>)>>

<ROUTINE EGG-OBJECT ()
	 <COND (<AND <VERB? OPEN MUNG> <EQUAL? ,PRSO ,EGG>>
		<COND (<FSET? ,PRSO ,OPENBIT>
		       <TELL "The egg is already open." CR>)
		      (<NOT ,PRSI>
		       <TELL "You have neither the tools nor the expertise."
			     CR>)
		      (<EQUAL? ,PRSI ,HANDS>
		       <TELL "I doubt you could do that without damaging it."
			     CR>)
		      (<OR <FSET? ,PRSI ,WEAPONBIT>
			   <FSET? ,PRSI ,TOOLBIT>
			   <VERB? MUNG>>
		       <TELL
"The egg is now open, but the clumsiness of your attempt has seriously
compromised its esthetic appeal.">
		       <BAD-EGG>
		       <CRLF>)
		      (<FSET? ,PRSO ,FIGHTBIT>
		       <TELL "Not to say that using the "
			     D ,PRSI
			     " isn't original too..." CR>)
		      (T
		       <TELL "The concept of using a "
			     D ,PRSI
			     " is certainly original." CR>
		       <FSET ,PRSO ,FIGHTBIT>)>)
	       (<VERB? CLIMB-ON HATCH>
		<TELL
"There is a noticeable crunch from beneath you, and inspection reveals
that the egg is lying open, badly damaged.">
		<BAD-EGG>
		<CRLF>)
	       (<VERB? OPEN MUNG THROW>
		<COND (<VERB? THROW> <MOVE ,PRSO ,HERE>)>
		<TELL
"Your rather indelicate handling of the egg has caused it some damage,
although you have succeeded in opening it.">
		<BAD-EGG>
		<CRLF>)>>

<ROUTINE BAD-EGG ("AUX" L)
	 <COND (<IN? ,CANARY ,EGG>
		<TELL " " <GETP ,BROKEN-CANARY ,P?FDESC>>)
	       (T <REMOVE-CAREFULLY ,BROKEN-CANARY>)>
	 <MOVE ,BROKEN-EGG <LOC ,EGG>>
	 <REMOVE-CAREFULLY ,EGG>
	 <RTRUE>>

<GLOBAL SING-SONG <>>

<ROUTINE CANARY-OBJECT ()
	 <COND (<VERB? WIND>
		<COND (<EQUAL? ,PRSO ,CANARY>
		       <COND (<AND <NOT ,SING-SONG> <FOREST-ROOM?>>
			      <TELL
"The canary chirps, slightly off-key, an aria from a forgotten opera.
From out of the greenery flies a lovely songbird. It perches on a
limb just over your head and opens its beak to sing. As it does so
a beautiful brass bauble drops from its mouth, bounces off the top of
your head, and lands glimmering in the grass. As the canary winds
down, the songbird flies away." CR>
			     <SETG SING-SONG T>
			     <MOVE ,BAUBLE
				   <COND (<EQUAL? ,HERE ,UP-A-TREE> ,PATH)
					 (T ,HERE)>>)
			    (T
			     <TELL
"The canary chirps blithely, if somewhat tinnily, for a short time." CR>)>)
		     (T
		      <TELL
"There is an unpleasant grinding noise from inside the canary." CR>)>)>>

<ROUTINE FOREST-ROOM? ()
	 <OR <EQUAL? ,HERE ,FOREST-1 ,FOREST-2 ,FOREST-3>
	     <EQUAL? ,HERE ,PATH ,UP-A-TREE>>>

<ROUTINE I-FOREST-ROOM ()
	 <COND (<NOT <FOREST-ROOM?>>
		<DISABLE <INT I-FOREST-ROOM>>
		<RFALSE>)
	       (<PROB 15>
		<TELL
"You hear in the distance the chirping of a song bird." CR>)>>

<ROUTINE FOREST-ROOM (RARG)
	 <COND (<EQUAL? .RARG ,M-ENTER> <ENABLE <QUEUE I-FOREST-ROOM -1>>)
	       (<EQUAL? .RARG ,M-BEG>
		<COND (<AND <VERB? CLIMB-FOO CLIMB-UP>
			    <EQUAL? ,PRSO ,TREE>>
		       <DO-WALK ,P?UP>)>)>>

<ROUTINE WCLIF-OBJECT ()
	 <COND (<VERB? CLIMB-UP CLIMB-DOWN CLIMB-FOO>
		<TELL "The cliff is too steep for climbing." CR>)>>

<ROUTINE CLIFF-OBJECT ()
	 <COND (<OR <VERB? LEAP>
		    <AND <VERB? PUT> <EQUAL? ,PRSO ,ME>>>
		<TELL
		 "That would be very unwise. Perhaps even fatal." CR>)
	       (<EQUAL? ,PRSI ,CLIMBABLE-CLIFF>
		<COND (<VERB? PUT THROW-OFF>
		       <TELL
"The " D ,PRSO " tumbles into the river and is seen no more." CR>
		       <REMOVE-CAREFULLY ,PRSO>)>)>>

\

"SUBTITLE CHUTES AND LADDERS"

<ROUTINE ROPE-FUNCTION ("AUX" RLOC)
	 <COND (<NOT <EQUAL? ,HERE ,DOME-ROOM>>
		<SETG DOME-FLAG <>>
		<COND (<VERB? TIE>
		       <TELL "You can't tie the rope to that." CR>)>)
	       (<VERB? TIE>
		<COND (<EQUAL? ,PRSI ,RAILING>
		       <COND (,DOME-FLAG
			      <TELL
			       "The rope is already tied to it." CR>)
			     (T
			      <TELL
"The rope drops over the side and comes within ten feet of the floor." CR>
			      <SETG DOME-FLAG T>
			      <FSET ,ROPE ,NDESCBIT>
			      <SET RLOC <LOC ,ROPE>>
			      <COND (<OR <NOT .RLOC>
					 <NOT <IN? .RLOC ,ROOMS>>>
				     <MOVE ,ROPE ,HERE>)>
			      T)>)>)
	       (<AND <VERB? CLIMB-DOWN> <EQUAL? ,PRSO ,ROPE ,ROOMS> ,DOME-FLAG>
		<DO-WALK ,P?DOWN>)
	       (<AND <VERB? TIE-UP>
		     <EQUAL? ,ROPE ,PRSI>>
		<COND (<FSET? ,PRSO ,ACTORBIT>
		       <COND (<L? <GETP ,PRSO ,P?STRENGTH> 0>
			      <TELL
"Your attempt to tie up the " D ,PRSO " awakens him.">
			      <AWAKEN ,PRSO>)
			     (T
			      <TELL
"The " D ,PRSO " struggles and you cannot tie him up." CR>)>)
		      (T
		       <TELL "Why would you tie up a " D ,PRSO "?" CR>)>)
	       (<VERB? UNTIE>
		<COND (,DOME-FLAG
		       <SETG DOME-FLAG <>>
		       <FCLEAR ,ROPE ,NDESCBIT>
		       <TELL "The rope is now untied." CR>)
		      (T
		       <TELL "It is not tied to anything." CR>)>)
	       (<AND <VERB? DROP>
		     <EQUAL? ,HERE ,DOME-ROOM>
		     <NOT ,DOME-FLAG>>
		<MOVE ,ROPE ,TORCH-ROOM>
		<TELL "The rope drops gently to the floor below." CR>)
	       (<VERB? TAKE>
		<COND (,DOME-FLAG
		       <TELL "The rope is tied to the railing." CR>)>)>>

<ROUTINE UNTIE-FROM ()
    <COND (<AND <EQUAL? ,PRSO ,ROPE>
		<AND ,DOME-FLAG <EQUAL? ,PRSI ,RAILING>>>
	   <PERFORM ,V?UNTIE ,PRSO>)
	  (T <TELL "It's not attached to that!" CR>)>>

<ROUTINE SLIDE-FUNCTION ()
	 <COND (<OR <VERB? THROUGH CLIMB-UP CLIMB-DOWN CLIMB-FOO>
		    <AND <VERB? PUT> <EQUAL? ,PRSO ,ME>>>
		<COND (<EQUAL? ,HERE ,CELLAR>
		       <DO-WALK ,P?WEST>
		       <RTRUE>)
		      (T
		       <TELL "You tumble down the slide...." CR>
		       <GOTO ,CELLAR>)>)
	       (<VERB? PUT>
		<SLIDER ,PRSO>)>>

<ROUTINE SLIDER (OBJ)
	 <COND (<FSET? .OBJ ,TAKEBIT>
		<TELL "The " D .OBJ " falls into the slide and is gone." CR>
		<COND (<EQUAL? .OBJ ,WATER> <REMOVE-CAREFULLY .OBJ>)
		      (T
		       <MOVE .OBJ ,CELLAR>)>)
	       (T <TELL <PICK-ONE ,YUKS> CR>)>>

<ROUTINE SANDWICH-BAG-FCN ()
	 <COND (<AND <VERB? SMELL>
		     <IN? ,LUNCH ,PRSO>>
		<TELL "It smells of hot peppers." CR>)>>

"MORE RANDOMNESS"

<ROUTINE DEAD-FUNCTION ("OPTIONAL" (FOO <>) "AUX" M)
	 <COND (<VERB? WALK>
		<COND (<AND <EQUAL? ,HERE ,TIMBER-ROOM>
			    <EQUAL? ,PRSO ,P?WEST>>
		       <TELL "You cannot enter in your condition." CR>)>)
	       (<VERB? BRIEF VERBOSE SUPER-BRIEF
		       VERSION ;AGAIN SAVE RESTORE QUIT RESTART>
		<>)
	       (<VERB? ATTACK MUNG ALARM SWING>
		<TELL "All such attacks are vain in your condition." CR>)
	       (<VERB? OPEN CLOSE EAT DRINK
		       INFLATE DEFLATE TURN BURN
		       TIE UNTIE RUB>
		<TELL
"Even such an action is beyond your capabilities." CR>)
	       (<VERB? WAIT>
		<TELL "Might as well. You've got an eternity." CR>)
	       (<VERB? LAMP-ON>
		<TELL "You need no light to guide you." CR>)
	       (<VERB? SCORE>
		<TELL "You're dead! How can you think of your score?" CR>)
	       (<VERB? TAKE RUB>
		<TELL "Your hand passes through its object." CR>)
	       (<VERB? DROP THROW INVENTORY>
		<TELL "You have no possessions." CR>)
	       (<VERB? DIAGNOSE>
		<TELL "You are dead." CR>)
	       (<VERB? LOOK>
		<TELL "The room looks strange and unearthly">
		<COND (<NOT <FIRST? ,HERE>>
		       <TELL ".">)
		      (T
		       <TELL " and objects appear indistinct.">)>
		<CRLF>
		<COND (<NOT <FSET? ,HERE ,ONBIT>>
		       <TELL
"Although there is no light, the room seems dimly illuminated." CR>)>
		<CRLF>
		<>)
	       (<VERB? PRAY>
		<COND (<EQUAL? ,HERE ,SOUTH-TEMPLE>
		       <FCLEAR ,LAMP ,INVISIBLE>
		       <PUTP ,WINNER ,P?ACTION 0>
		       ;<SETG GWIM-DISABLE <>>
		       <SETG ALWAYS-LIT <>>
		       <SETG DEAD <>>
		       <COND (<IN? ,TROLL ,TROLL-ROOM>
			      <SETG TROLL-FLAG <>>)>
		       <TELL
"From the distance the sound of a lone trumpet is heard. The room
becomes very bright and you feel disembodied. In a moment, the
brightness fades and you find yourself rising as if from a long
sleep, deep in the woods. In the distance you can faintly hear a
songbird and the sounds of the forest." CR CR>
		       <GOTO ,FOREST-1>)
		      (T
		       <TELL "Your prayers are not heard." CR>)>)
	       (T
		<TELL "You can't even do that." CR>
		<SETG P-CONT <>>
		<RFATAL>)>>

;"Pseudo-object routines"

<ROUTINE LAKE-PSEUDO ()
	 <COND (,LOW-TIDE
		<TELL "There's not much lake left...." CR>)
	       (<VERB? CROSS>
		<TELL "It's too wide to cross." CR>)
	       (<VERB? THROUGH>
		<TELL "You can't swim in this lake." CR>)>>

<ROUTINE STREAM-PSEUDO ()
	 <COND (<VERB? SWIM THROUGH>
		<TELL "You can't swim in the stream." CR>)
	       (<VERB? CROSS>
		<TELL "The other side is a sheer rock cliff." CR>)>>

<ROUTINE CHASM-PSEUDO ()
	 <COND (<OR <VERB? LEAP>
		    <AND <VERB? PUT> <EQUAL? ,PRSO ,ME>>>
		<TELL
"You look before leaping, and realize that you would never survive." CR>)
	       (<VERB? CROSS>
		<TELL "It's too far to jump, and there's no bridge." CR>)
	       (<AND <VERB? PUT THROW-OFF> <EQUAL? ,PRSI ,PSEUDO-OBJECT>>
		<TELL
"The " D ,PRSO " drops out of sight into the chasm." CR>
		<REMOVE-CAREFULLY ,PRSO>)>>

<ROUTINE DOME-PSEUDO ()
	 <COND (<VERB? KISS>
		<TELL "No." CR>)>>

<ROUTINE GATE-PSEUDO ()
	 <COND (<VERB? THROUGH>
		<DO-WALK ,P?IN>
		<RTRUE>)
	       (T
		<TELL
"The gate is protected by an invisible force. It makes your
teeth ache to touch it." CR>)>>

<ROUTINE DOOR-PSEUDO () ;"in Studio"
	 <COND (<VERB? OPEN CLOSE>
		<TELL "The door won't budge." CR>)
	       (<VERB? THROUGH>
		<DO-WALK ,P?SOUTH>)>>

<ROUTINE PAINT-PSEUDO ()
	 <COND (<VERB? MUNG>
		<TELL "Some paint chips away, revealing more paint." CR>)>>

<ROUTINE GAS-PSEUDO ()
	 <COND (<VERB? BREATHE>	;"REALLY BLOW"
		<TELL "There is too much gas to blow away." CR>)
	       (<VERB? SMELL>
		<TELL "It smells like coal gas in here." CR>)>>

"SUBTITLE MELEE"

"melee actions (object functions for villains called with these"

<CONSTANT F-BUSY? 1>		;"busy recovering weapon?"
<CONSTANT F-DEAD 2>		;"mistah kurtz, he dead."
<CONSTANT F-UNCONSCIOUS 3>	;"into dreamland"
<CONSTANT F-CONSCIOUS 4>	;"rise and shine"
<CONSTANT F-FIRST? 5>		;"strike first?"

\

"blow results"

<CONSTANT MISSED 1>		;"attacker misses"
<CONSTANT UNCONSCIOUS 2>	;"defender unconscious"
<CONSTANT KILLED 3>		;"defender dead"
<CONSTANT LIGHT-WOUND 4>	;"defender lightly wounded"
<CONSTANT SERIOUS-WOUND 5>	;"defender seriously wounded"
<CONSTANT STAGGER 6>		;"defender staggered (miss turn)"
<CONSTANT LOSE-WEAPON 7>	;"defender loses weapon"
<CONSTANT HESITATE 8>		;"hesitates (miss on free swing)"
<CONSTANT SITTING-DUCK 9>	;"sitting duck (crunch!)"

"tables of melee results"

<GLOBAL DEF1
	<TABLE (PURE)
	 MISSED MISSED MISSED MISSED
	 STAGGER STAGGER
	 UNCONSCIOUS UNCONSCIOUS
	 KILLED KILLED KILLED KILLED KILLED>>

<GLOBAL DEF2A
	<TABLE (PURE)
	 MISSED MISSED MISSED MISSED MISSED
	 STAGGER STAGGER
	 LIGHT-WOUND LIGHT-WOUND
	 UNCONSCIOUS>>

<GLOBAL DEF2B
	<TABLE (PURE)
	 MISSED MISSED MISSED
	 STAGGER STAGGER
	 LIGHT-WOUND LIGHT-WOUND LIGHT-WOUND
	 UNCONSCIOUS
	 KILLED KILLED KILLED>>

<GLOBAL DEF3A
	<TABLE (PURE)
	 MISSED MISSED MISSED MISSED MISSED
	 STAGGER STAGGER
	 LIGHT-WOUND LIGHT-WOUND
	 SERIOUS-WOUND SERIOUS-WOUND>>

<GLOBAL DEF3B
	<TABLE (PURE)
	 MISSED MISSED MISSED
	 STAGGER STAGGER
	 LIGHT-WOUND LIGHT-WOUND LIGHT-WOUND
	 SERIOUS-WOUND SERIOUS-WOUND SERIOUS-WOUND>>

<GLOBAL DEF3C
	<TABLE (PURE)
	 MISSED
	 STAGGER STAGGER
	 LIGHT-WOUND LIGHT-WOUND LIGHT-WOUND LIGHT-WOUND
	 SERIOUS-WOUND SERIOUS-WOUND SERIOUS-WOUND>>

<GLOBAL DEF1-RES
	<TABLE DEF1
	       0 ;<REST ,DEF1 2>
	       0 ;<REST ,DEF1 4>>>

<GLOBAL DEF2-RES
	<TABLE DEF2A
	       DEF2B
	       0; <REST ,DEF2B 2>
	       0; <REST ,DEF2B 4>>>

<GLOBAL DEF3-RES
	<TABLE DEF3A
	       0 ;<REST ,DEF3A 2>
	       DEF3B
	       0 ;<REST ,DEF3B 2>
	       DEF3C>>

\

"useful constants"

<CONSTANT STRENGTH-MAX 7>
<CONSTANT STRENGTH-MIN 2>
<CONSTANT CURE-WAIT 30>

\

"I-FIGHT moved to DEMONS"

<ROUTINE DO-FIGHT (LEN "AUX" CNT RES O OO (OUT <>))
	<REPEAT ()
	      <SET CNT 0>
	      <REPEAT ()
		      <SET CNT <+ .CNT 1>>
		      <COND (<EQUAL? .CNT .LEN>
			     <SET RES T>
			     <RETURN T>)>
		      <SET OO <GET ,VILLAINS .CNT>>
		      <SET O <GET .OO ,V-VILLAIN>>
		      <COND (<NOT <FSET? .O ,FIGHTBIT>>)
			    (<APPLY <GETP .O ,P?ACTION>
				    ,F-BUSY?>)
			    (<NOT <SET RES
				       <VILLAIN-BLOW
					.OO
					.OUT>>>
			     <SET RES <>>
			     <RETURN>)
			    (<EQUAL? .RES ,UNCONSCIOUS>
			     <SET OUT <+ 1 <RANDOM 3>>>)>>
	      <COND (.RES
		     <COND (<NOT .OUT> <RETURN>)
			   (T
			    <SET OUT <- .OUT 1>>
			    <COND (<0? .OUT> <RETURN>)>)>)
		    (T <RETURN>)>>>

\

"takes a remark, defender, and good-guy's weapon"

<ROUTINE REMARK (REMARK D W "AUX" (LEN <GET .REMARK 0>) (CNT 0) STR)
	 <REPEAT ()
	         <COND (<G? <SET CNT <+ .CNT 1>> .LEN> <RETURN>)>
		 <SET STR <GET .REMARK .CNT>>
		 <COND (<EQUAL? .STR ,F-WEP> <PRINTD .W>)
		       (<EQUAL? .STR ,F-DEF> <PRINTD .D>)
		       (T <PRINT .STR>)>>
	 <CRLF>>

"Strength of the player is a basic value (S) adjusted by his P?STRENGTH
property, which is normally 0"

<ROUTINE FIGHT-STRENGTH ("OPTIONAL" (ADJUST? T) "AUX" S)
	 <SET S
	      <+ ,STRENGTH-MIN
		 </ ,SCORE
		    </ ,SCORE-MAX
		       <- ,STRENGTH-MAX ,STRENGTH-MIN>>>>>
	 <COND (.ADJUST? <+ .S <GETP ,WINNER ,P?STRENGTH>>)(T .S)>>

<ROUTINE VILLAIN-STRENGTH (OO
			   "AUX" (VILLAIN <GET .OO ,V-VILLAIN>)
			   OD TMP)
	 <SET OD <GETP .VILLAIN ,P?STRENGTH>>
	 <COND (<NOT <L? .OD 0>>
		<COND (<AND <EQUAL? .VILLAIN ,THIEF> ,THIEF-ENGROSSED>
		       <COND (<G? .OD 2> <SET OD 2>)>
		       <SETG THIEF-ENGROSSED <>>)>
		<COND (<AND ,PRSI
			    <FSET? ,PRSI ,WEAPONBIT>
			    <EQUAL? <GET .OO ,V-BEST> ,PRSI>>
		       <SET TMP <- .OD <GET .OO ,V-BEST-ADV>>>
		       <COND (<L? .TMP 1> <SET TMP 1>)>
		       <SET OD .TMP>)>)>
	 .OD>

"find a weapon (if any) in possession of argument"

<ROUTINE FIND-WEAPON (O "AUX" W)
	 <SET W <FIRST? .O>>
	 <COND (<NOT .W>
		<RFALSE>)>
	 <REPEAT ()
		 <COND (<OR <EQUAL? .W ,STILETTO ,AXE ,SWORD>
			    <EQUAL? .W ,KNIFE ,RUSTY-KNIFE>>
			<RETURN .W>)
		       (<NOT <SET W <NEXT? .W>>> <RFALSE>)>>>

\

<ROUTINE VILLAIN-BLOW (OO OUT?
		       "AUX" (VILLAIN <GET .OO ,V-VILLAIN>)
		       (REMARKS <GET .OO ,V-MSGS>)
		       DWEAPON ATT DEF OA OD TBL RES NWEAPON)
	 <FCLEAR ,WINNER ,STAGGERED>
	 <COND (<FSET? .VILLAIN ,STAGGERED>
		<TELL "The " D .VILLAIN
		      " slowly regains his feet." CR>
		<FCLEAR .VILLAIN ,STAGGERED>
		<RTRUE>)>
	 <SET OA <SET ATT <VILLAIN-STRENGTH .OO>>>
	 <COND (<NOT <G? <SET DEF <FIGHT-STRENGTH>> 0>> <RTRUE>)>
	 <SET OD <FIGHT-STRENGTH <>>>
	 <SET DWEAPON <FIND-WEAPON ,WINNER>>
	 <COND (<L? .DEF 0> <SET RES ,KILLED>)
	       (T
		<COND (<1? .DEF>
		       <COND (<G? .ATT 2> <SET ATT 3>)>
		       <SET TBL <GET ,DEF1-RES <- .ATT 1>>>)
		      (<EQUAL? .DEF 2>
		       <COND (<G? .ATT 3> <SET ATT 4>)>
		       <SET TBL <GET ,DEF2-RES <- .ATT 1>>>)
		      (<G? .DEF 2>
		       <SET ATT <- .ATT .DEF>>
		       <COND (<L? .ATT -1> <SET ATT -2>)
			     (<G? .ATT 1> <SET ATT 2>)>
		       <SET TBL <GET ,DEF3-RES <+ .ATT 2>>>)>
		<SET RES <GET .TBL <- <RANDOM 9> 1>>>
		<COND (.OUT?
		       <COND (<EQUAL? .RES ,STAGGER> <SET RES ,HESITATE>)
			     (T <SET RES ,SITTING-DUCK>)>)>
		<COND (<AND <EQUAL? .RES ,STAGGER>
			    .DWEAPON
			    <PROB 25 <COND (.HERO? 10)(T 50)>>>
		       <SET RES ,LOSE-WEAPON>)>
		<REMARK
		  <RANDOM-ELEMENT <GET .REMARKS <- .RES 1>>>
		  ,WINNER
		  .DWEAPON>)>
	 <COND (<OR <EQUAL? .RES ,MISSED> <EQUAL? .RES ,HESITATE>>)
	       (<EQUAL? .RES ,UNCONSCIOUS>)
	       (<OR <EQUAL? .RES ,KILLED>
		    <EQUAL? .RES ,SITTING-DUCK>>
		<SET DEF 0>)
	       (<EQUAL? .RES ,LIGHT-WOUND>
		<SET DEF <- .DEF 1>>
		<COND (<L? .DEF 0> <SET DEF 0>)>
		<COND (<G? ,LOAD-ALLOWED 50>
		       <SETG LOAD-ALLOWED <- ,LOAD-ALLOWED 10>>)>)
	       (<EQUAL? .RES ,SERIOUS-WOUND>
		<SET DEF <- .DEF 2>>
		<COND (<L? .DEF 0> <SET DEF 0>)>
		<COND (<G? ,LOAD-ALLOWED 50>
		       <SETG LOAD-ALLOWED <- ,LOAD-ALLOWED 20>>)>)
	       (<EQUAL? .RES ,STAGGER> <FSET ,WINNER ,STAGGERED>)
	       (T
		;<AND <EQUAL? .RES ,LOSE-WEAPON> .DWEAPON>
		<MOVE .DWEAPON ,HERE>
		<COND (<SET NWEAPON <FIND-WEAPON ,WINNER>>
		       <TELL
"Fortunately, you still have a " D .NWEAPON "." CR>)>)>
	 <WINNER-RESULT .DEF .RES .OD>>

<ROUTINE HERO-BLOW ("AUX" OO VILLAIN (OUT? <>) DWEAPON ATT DEF (CNT 0)
		    OA OD TBL RES NWEAPON (LEN <GET ,VILLAINS 0>))
	 <REPEAT ()
		 <SET CNT <+ .CNT 1>>
		 <COND (<EQUAL? .CNT .LEN> <RETURN>)>
		 <SET OO <GET ,VILLAINS .CNT>>
		 <COND (<EQUAL? <GET .OO ,V-VILLAIN> ,PRSO>
			<RETURN>)>>
	 <FSET ,PRSO ,FIGHTBIT>
	 <COND (<FSET? ,WINNER ,STAGGERED>
		<TELL
"You are still recovering from that last blow, so your attack is
ineffective." CR>
		<FCLEAR ,WINNER ,STAGGERED>
		<RTRUE>)>
	 <SET ATT <FIGHT-STRENGTH>>
	 <COND (<L? .ATT 1> <SET ATT 1>)>
	 <SET OA .ATT>
	 <SET VILLAIN <GET .OO ,V-VILLAIN>>
	 <COND (<0? <SET OD <SET DEF <VILLAIN-STRENGTH .OO>>>>
		<COND (<EQUAL? ,PRSO ,WINNER>
		       <RETURN <JIGS-UP
"Well, you really did it that time. Is suicide painless?">>)>
		<TELL "Attacking the " D .VILLAIN " is pointless." CR>
		<RTRUE>)>
	 <SET DWEAPON <FIND-WEAPON .VILLAIN>>
	 <COND (<OR <NOT .DWEAPON> <L? .DEF 0>>
		<TELL "The ">
		<COND (<L? .DEF 0> <TELL "unconscious">)
		      (T <TELL "unarmed">)>
		<TELL " " D .VILLAIN
		      " cannot defend himself: He dies." CR>
		<SET RES ,KILLED>)
	       (T
		<COND (<1? .DEF>
		       <COND (<G? .ATT 2> <SET ATT 3>)>
		       <SET TBL <GET ,DEF1-RES <- .ATT 1>>>)
		      (<EQUAL? .DEF 2>
		       <COND (<G? .ATT 3> <SET ATT 4>)>
		       <SET TBL <GET ,DEF2-RES <- .ATT 1>>>)
		      (<G? .DEF 2>
		       <SET ATT <- .ATT .DEF>>
		       <COND (<L? .ATT -1> <SET ATT -2>)
			     (<G? .ATT 1> <SET ATT 2>)>
		       <SET TBL <GET ,DEF3-RES <+ .ATT 2>>>)>
		<SET RES <GET .TBL <- <RANDOM 9> 1>>>
		<COND (.OUT?
		       <COND (<EQUAL? .RES ,STAGGER> <SET RES ,HESITATE>)
			     (T <SET RES ,SITTING-DUCK>)>)>
		<COND (<AND <EQUAL? .RES ,STAGGER> .DWEAPON <PROB 25>>
		       <SET RES ,LOSE-WEAPON>)>
		<REMARK
		  <RANDOM-ELEMENT <GET ,HERO-MELEE <- .RES 1>>>
		  ,PRSO
		  ,PRSI>)>
	 <COND (<OR <EQUAL? .RES ,MISSED> <EQUAL? .RES ,HESITATE>>)
	       (<EQUAL? .RES ,UNCONSCIOUS> <SET DEF <- .DEF>>)
	       (<OR <EQUAL? .RES ,KILLED> <EQUAL? .RES ,SITTING-DUCK>>
		<SET DEF 0>)
	       (<EQUAL? .RES ,LIGHT-WOUND>
		<SET DEF <- .DEF 1>>
		<COND (<L? .DEF 0> <SET DEF 0>)>)
	       (<EQUAL? .RES ,SERIOUS-WOUND>
		<SET DEF <- .DEF 2>>
		<COND (<L? .DEF 0> <SET DEF 0>)>)
	       (<EQUAL? .RES ,STAGGER> <FSET ,PRSO ,STAGGERED>)
	       (T
		;<AND <EQUAL? .RES ,LOSE-WEAPON> .DWEAPON>
		<FCLEAR .DWEAPON ,NDESCBIT>
		<FSET .DWEAPON ,WEAPONBIT>
		<MOVE .DWEAPON ,HERE>
		<THIS-IS-IT .DWEAPON>)>
	 <VILLAIN-RESULT ,PRSO .DEF .RES>>

\

<ROUTINE WINNER-RESULT (DEF RES OD)
	 <PUTP ,WINNER
	       ,P?STRENGTH
	       <COND (<0? .DEF> -10000)(T <- .DEF .OD>)>>
	 <COND (<L? <- .DEF .OD> 0>
		<ENABLE <QUEUE I-CURE ,CURE-WAIT>>)>
	 <COND (<NOT <G? <FIGHT-STRENGTH> 0>>
		<PUTP ,WINNER ,P?STRENGTH <+ 1 <- <FIGHT-STRENGTH <>>>>>
		<JIGS-UP
"It appears that that last blow was too much for you. I'm afraid you
are dead.">
		<>)
	       (T .RES)>>

<ROUTINE VILLAIN-RESULT (VILLAIN DEF RES)
	 <PUTP .VILLAIN ,P?STRENGTH .DEF>
	 <COND (<0? .DEF>
		<FCLEAR .VILLAIN ,FIGHTBIT>
		<TELL
"Almost as soon as the " D .VILLAIN " breathes his last breath, a cloud
of sinister black fog envelops him, and when the fog lifts, the
carcass has disappeared." CR>
		<REMOVE-CAREFULLY .VILLAIN>
		<APPLY <GETP .VILLAIN ,P?ACTION> ,F-DEAD>
		.RES)
	       (<EQUAL? .RES ,UNCONSCIOUS>
		<APPLY <GETP .VILLAIN ,P?ACTION> ,F-UNCONSCIOUS>
		.RES)
	       (T .RES)>>

\

<ROUTINE WINNING? (V "AUX" VS PS)
	 <SET VS <GETP .V ,P?STRENGTH>>
	 <SET PS <- .VS <FIGHT-STRENGTH>>>
	 <COND (<G? .PS 3> <PROB 90>)
	       (<G? .PS 0> <PROB 75>)
	       (<0? .PS> <PROB 50>)
	       (<G? .VS 1> <PROB 25>)
	       (T <PROB 10>)>>

<ROUTINE I-CURE ("AUX" (S <GETP ,WINNER ,P?STRENGTH>))
	 <COND (<G? .S 0> <SET S 0> <PUTP ,WINNER ,P?STRENGTH .S>)
	       (<L? .S 0> <SET S <+ .S 1>> <PUTP ,WINNER ,P?STRENGTH .S>)>
	 <COND (<L? .S 0>
		<COND (<L? ,LOAD-ALLOWED ,LOAD-MAX>
		       <SETG LOAD-ALLOWED <+ ,LOAD-ALLOWED 10>>)>
		<ENABLE <QUEUE I-CURE ,CURE-WAIT>>)
	       (T
		<SETG LOAD-ALLOWED ,LOAD-MAX>
		<DISABLE <INT I-CURE>>)>>

"FIGHTS"

"messages for winner"

<CONSTANT F-WEP 0>	;"means print weapon name"
<CONSTANT F-DEF 1>	;"means print defender name (villain, e.g.)"

<GLOBAL HERO-MELEE
 <TABLE (PURE)
  <LTABLE (PURE)
   <LTABLE (PURE) "Your " F-WEP " misses the " F-DEF " by an inch.">
   <LTABLE (PURE) "A good slash, but it misses the " F-DEF " by a mile.">
   <LTABLE (PURE) "You charge, but the " F-DEF " jumps nimbly aside.">
   <LTABLE (PURE) "Clang! Crash! The " F-DEF " parries.">
   <LTABLE (PURE) "A quick stroke, but the " F-DEF " is on guard.">
   <LTABLE (PURE) "A good stroke, but it's too slow; the " F-DEF " dodges.">>
  <LTABLE (PURE)
   <LTABLE (PURE) "Your " F-WEP " crashes down, knocking the " F-DEF " into dreamland.">
   <LTABLE (PURE) "The " F-DEF " is battered into unconsciousness.">
   <LTABLE (PURE) "A furious exchange, and the " F-DEF " is knocked out!">
   <LTABLE (PURE) "The haft of your " F-WEP " knocks out the " F-DEF ".">
   <LTABLE (PURE) "The " F-DEF " is knocked out!">>
  <LTABLE (PURE)
   <LTABLE (PURE) "It's curtains for the " F-DEF " as your " F-WEP " removes his head.">
   <LTABLE (PURE) "The fatal blow strikes the " F-DEF " square in the heart: He dies.">
   <LTABLE (PURE) "The " F-DEF " takes a fatal blow and slumps to the floor dead.">>
  <LTABLE (PURE)
   <LTABLE (PURE) "The " F-DEF " is struck on the arm; blood begins to trickle down.">
   <LTABLE (PURE) "Your " F-WEP " pinks the " F-DEF " on the wrist, but it's not serious.">
   <LTABLE (PURE) "Your stroke lands, but it was only the flat of the blade.">
   <LTABLE (PURE) "The blow lands, making a shallow gash in the " F-DEF "'s arm!">>
  <LTABLE (PURE)
   <LTABLE (PURE) "The " F-DEF " receives a deep gash in his side.">
   <LTABLE (PURE) "A savage blow on the thigh! The " F-DEF " is stunned but can still fight!">
   <LTABLE (PURE) "Slash! Your blow lands! That one hit an artery, it could be serious!">
   <LTABLE (PURE) "Slash! Your stroke connects! This could be serious!">>
  <LTABLE (PURE)
   <LTABLE (PURE) "The " F-DEF " is staggered, and drops to his knees.">
   <LTABLE (PURE) "The " F-DEF " is momentarily disoriented and can't fight back.">
   <LTABLE (PURE) "The force of your blow knocks the " F-DEF " back, stunned.">
   <LTABLE (PURE) "The " F-DEF " is confused and can't fight back.">
   <LTABLE (PURE) "The quickness of your thrust knocks the " F-DEF " back, stunned.">>
  <LTABLE (PURE)
   <LTABLE (PURE) "The " F-DEF "'s weapon is knocked to the floor, leaving him unarmed.">
   <LTABLE (PURE) "The " F-DEF " is disarmed by a subtle feint past his guard.">>>>

\

"messages for cyclops (note that he has no weapon"

<GLOBAL CYCLOPS-MELEE
 <TABLE (PURE)
  <LTABLE (PURE)
   <LTABLE (PURE) "The Cyclops misses, but the backwash almost knocks you over.">
   <LTABLE (PURE) "The Cyclops rushes you, but runs into the wall.">>
  <LTABLE (PURE)
   <LTABLE (PURE) "The Cyclops sends you crashing to the floor, unconscious.">>
  <LTABLE (PURE)
   <LTABLE (PURE) "The Cyclops breaks your neck with a massive smash.">>
  <LTABLE (PURE)
   <LTABLE (PURE) "A quick punch, but it was only a glancing blow.">
   <LTABLE (PURE) "A glancing blow from the Cyclops' fist.">>
  <LTABLE (PURE)
   <LTABLE (PURE) "The monster smashes his huge fist into your chest, breaking several
ribs.">
   <LTABLE (PURE) "The Cyclops almost knocks the wind out of you with a quick punch.">>
  <LTABLE (PURE)
   <LTABLE (PURE) "The Cyclops lands a punch that knocks the wind out of you.">
   <LTABLE (PURE) "Heedless of your weapons, the Cyclops tosses you against the rock
wall of the room.">>
  <LTABLE (PURE)
   <LTABLE (PURE) "The Cyclops grabs your " F-WEP ", tastes it, and throws it to the
ground in disgust.">
   <LTABLE (PURE) "The monster grabs you on the wrist, squeezes, and you drop your
" F-WEP " in pain.">>
  <LTABLE (PURE)
   <LTABLE (PURE) "The Cyclops seems unable to decide whether to broil or stew his
dinner.">>
  <LTABLE (PURE)
   <LTABLE (PURE) "The Cyclops, no sportsman, dispatches his unconscious victim.">>>>

\

"messages for troll"

<GLOBAL TROLL-MELEE
<TABLE (PURE)
 <LTABLE (PURE)
  <LTABLE (PURE) "The troll swings his axe, but it misses.">
  <LTABLE (PURE) "The troll's axe barely misses your ear.">
  <LTABLE (PURE) "The axe sweeps past as you jump aside.">
  <LTABLE (PURE) "The axe crashes against the rock, throwing sparks!">>
 <LTABLE (PURE)
  <LTABLE (PURE) "The flat of the troll's axe hits you delicately on the head, knocking
you out.">>
 <LTABLE (PURE)
  <LTABLE (PURE) "The troll neatly removes your head.">
  <LTABLE (PURE) "The troll's axe stroke cleaves you from the nave to the chops.">
  <LTABLE (PURE) "The troll's axe removes your head.">>
 <LTABLE (PURE)
  <LTABLE (PURE) "The axe gets you right in the side. Ouch!">
  <LTABLE (PURE) "The flat of the troll's axe skins across your forearm.">
  <LTABLE (PURE) "The troll's swing almost knocks you over as you barely parry
in time.">
  <LTABLE (PURE) "The troll swings his axe, and it nicks your arm as you dodge.">>
 <LTABLE (PURE)
  <LTABLE (PURE) "The troll charges, and his axe slashes you on your " F-WEP " arm.">
  <LTABLE (PURE) "An axe stroke makes a deep wound in your leg.">
  <LTABLE (PURE) "The troll's axe swings down, gashing your shoulder.">>
 <LTABLE (PURE)
  <LTABLE (PURE) "The troll hits you with a glancing blow, and you are momentarily
stunned.">
  <LTABLE (PURE) "The troll swings; the blade turns on your armor but crashes
broadside into your head.">
  <LTABLE (PURE) "You stagger back under a hail of axe strokes.">
  <LTABLE (PURE) "The troll's mighty blow drops you to your knees.">>
 <LTABLE (PURE)
  <LTABLE (PURE) "The axe hits your " F-WEP " and knocks it spinning.">
  <LTABLE (PURE) "The troll swings, you parry, but the force of his blow knocks your " F-WEP " away.">
  <LTABLE (PURE) "The axe knocks your " F-WEP " out of your hand. It falls to the floor.">>
 <LTABLE (PURE)
  <LTABLE (PURE) "The troll hesitates, fingering his axe.">
  <LTABLE (PURE) "The troll scratches his head ruminatively:  Might you be magically
protected, he wonders?">>
 <LTABLE (PURE)
  <LTABLE (PURE) "Conquering his fears, the troll puts you to death.">>>>

\

"messages for thief"

<GLOBAL THIEF-MELEE
<TABLE (PURE)
 <LTABLE (PURE)
  <LTABLE (PURE) "The thief stabs nonchalantly with his stiletto and misses.">
  <LTABLE (PURE) "You dodge as the thief comes in low.">
  <LTABLE (PURE) "You parry a lightning thrust, and the thief salutes you with
a grim nod.">
  <LTABLE (PURE) "The thief tries to sneak past your guard, but you twist away.">>
 <LTABLE (PURE)
  <LTABLE (PURE) "Shifting in the midst of a thrust, the thief knocks you unconscious
with the haft of his stiletto.">
  <LTABLE (PURE) "The thief knocks you out.">>
 <LTABLE (PURE)
  <LTABLE (PURE) "Finishing you off, the thief inserts his blade into your heart.">
  <LTABLE (PURE) "The thief comes in from the side, feints, and inserts the blade
into your ribs.">
  <LTABLE (PURE) "The thief bows formally, raises his stiletto, and with a wry grin,
ends the battle and your life.">>
 <LTABLE (PURE)
  <LTABLE (PURE) "A quick thrust pinks your left arm, and blood starts to
trickle down.">
  <LTABLE (PURE) "The thief draws blood, raking his stiletto across your arm.">
  <LTABLE (PURE) "The stiletto flashes faster than you can follow, and blood wells
from your leg.">
  <LTABLE (PURE) "The thief slowly approaches, strikes like a snake, and leaves
you wounded.">>
 <LTABLE (PURE)
  <LTABLE (PURE) "The thief strikes like a snake! The resulting wound is serious.">
  <LTABLE (PURE) "The thief stabs a deep cut in your upper arm.">
  <LTABLE (PURE) "The stiletto touches your forehead, and the blood obscures your
vision.">
  <LTABLE (PURE) "The thief strikes at your wrist, and suddenly your grip is slippery
with blood.">>
 <LTABLE (PURE)
  <LTABLE (PURE) "The butt of his stiletto cracks you on the skull, and you stagger
back.">
  <LTABLE (PURE) "The thief rams the haft of his blade into your stomach, leaving
you out of breath.">
  <LTABLE (PURE) "The thief attacks, and you fall back desperately.">>
 <LTABLE (PURE)
  <LTABLE (PURE) "A long, theatrical slash. You catch it on your " F-WEP ", but the
thief twists his knife, and the " F-WEP " goes flying.">
  <LTABLE (PURE) "The thief neatly flips your " F-WEP " out of your hands, and it drops
to the floor.">
  <LTABLE (PURE) "You parry a low thrust, and your " F-WEP " slips out of your hand.">>
 <LTABLE (PURE)
  <LTABLE (PURE) "The thief, a man of superior breeding, pauses for a moment to consider the propriety of finishing you off.">
  <LTABLE (PURE) "The thief amuses himself by searching your pockets.">
  <LTABLE (PURE) "The thief entertains himself by rifling your pack.">>
 <LTABLE (PURE)
  <LTABLE (PURE) "The thief, forgetting his essentially genteel upbringing, cuts your
throat.">
  <LTABLE (PURE) "The thief, a pragmatist, dispatches you as a threat to his
livelihood.">>>>


"each table entry is:"

<CONSTANT V-VILLAIN 0>	;"villain"
<CONSTANT V-BEST 1>	;"best weapon"
<CONSTANT V-BEST-ADV 2>	;"advantage it confers"
<CONSTANT V-PROB 3>	;"prob of waking if unconscious"
<CONSTANT V-MSGS 4>	;"messages for that villain"

"This table must be after TROLL-MELEE, THIEF-MELEE, CYCLOPS-MELEE defined!"

<GLOBAL VILLAINS
	<LTABLE <TABLE TROLL SWORD 1 0 TROLL-MELEE>
		<TABLE THIEF KNIFE 1 0 THIEF-MELEE>
		<TABLE CYCLOPS <> 0 0 CYCLOPS-MELEE>>>

"DEMONS"

"Fighting demon"

<ROUTINE I-FIGHT ("AUX" (FIGHT? <>) (LEN <GET ,VILLAINS 0>)
		  CNT OO O P)
      <COND (,DEAD <RFALSE>)>
      <SET CNT 0>
      <REPEAT ()
	      <SET CNT <+ .CNT 1>>
	      <COND (<EQUAL? .CNT .LEN> <RETURN>)>
	      <SET OO <GET ,VILLAINS .CNT>>
	      <COND (<AND <IN? <SET O <GET .OO ,V-VILLAIN>> ,HERE>
			  <NOT <FSET? .O ,INVISIBLE>>>
		     <COND (<AND <EQUAL? .O ,THIEF> ,THIEF-ENGROSSED>
			    <SETG THIEF-ENGROSSED <>>)
			   (<L? <GETP .O ,P?STRENGTH> 0>
			    <SET P <GET .OO ,V-PROB>>
			    <COND (<AND <NOT <0? .P>> <PROB .P>>
				   <PUT .OO ,V-PROB 0>
				   <AWAKEN .O>)
				  (T
				   <PUT .OO ,V-PROB <+ .P 25>>)>)
			   (<OR <FSET? .O ,FIGHTBIT>
				<APPLY <GETP .O ,P?ACTION> ,F-FIRST?>>
			    <SET FIGHT? T>)>)
		    (T
		     <COND (<FSET? .O ,FIGHTBIT>
			    <APPLY <GETP .O ,P?ACTION> ,F-BUSY?>)>
		     <COND (<EQUAL? .O ,THIEF> <SETG THIEF-ENGROSSED <>>)>
		     <FCLEAR ,WINNER ,STAGGERED>
		     <FCLEAR .O ,STAGGERED>
		     <FCLEAR .O ,FIGHTBIT>
		     <AWAKEN .O>)>>
      <COND (<NOT .FIGHT?> <RFALSE>)>
      <DO-FIGHT .LEN>>

<ROUTINE AWAKEN (O "AUX" (S <GETP .O ,P?STRENGTH>))
	 <COND (<L? .S 0>
		<PUTP .O ,P?STRENGTH <- 0 .S>>
		<APPLY <GETP .O ,P?ACTION> ,F-CONSCIOUS>)>
	 T>

"SWORD demon"

<ROUTINE I-SWORD ("AUX" (DEM <INT I-SWORD>) (G <GETP ,SWORD ,P?TVALUE>)
		        (NG 0) P T L)
	 <COND (<IN? ,SWORD ,ADVENTURER>
		<COND (<INFESTED? ,HERE> <SET NG 2>)
		      (T
		       <SET P 0>
		       <REPEAT ()
			       <COND (<0? <SET P <NEXTP ,HERE .P>>>
				      <RETURN>)
				     (<NOT <L? .P ,LOW-DIRECTION>>
				      <SET T <GETPT ,HERE .P>>
				      <SET L <PTSIZE .T>>
				      <COND (<EQUAL? .L ,UEXIT ,CEXIT ,DEXIT>
					     <COND (<INFESTED? <GETB .T 0>>
						    <SET NG 1>
						    <RETURN>)>)>)>>)>
		<COND (<EQUAL? .NG .G> <RFALSE>)
		      (<EQUAL? .NG 2>
		       <TELL "Your sword has begun to glow very brightly." CR>)
		      (<1? .NG>
		       <TELL "Your sword is glowing with a faint blue glow."
			     CR>)
		      (<0? .NG>
		       <TELL "Your sword is no longer glowing." CR>)>
		<PUTP ,SWORD ,P?TVALUE .NG>
		<RTRUE>)
	       (T
		<PUT .DEM ,C-ENABLED? 0>
		<RFALSE>)>>

<ROUTINE INFESTED? (R "AUX" (F <FIRST? .R>))
	 <REPEAT ()
		 <COND (<NOT .F> <RFALSE>)
		       (<AND <FSET? .F ,ACTORBIT> <NOT <FSET? .F ,INVISIBLE>>>
			<RTRUE>)
		       (<NOT <SET F <NEXT? .F>>> <RFALSE>)>>>

"THIEF demon"

<ROUTINE I-THIEF ("AUX" (RM <LOC ,THIEF>) ROBJ HERE? (ONCE <>) (FLG <>))
   <PROG ()
     <COND (<SET HERE? <NOT <FSET? ,THIEF ,INVISIBLE>>>
	    <SET RM <LOC ,THIEF>>)>
     <COND
      (<AND <EQUAL? .RM ,TREASURE-ROOM> <NOT <EQUAL? .RM ,HERE>>>
       <COND (.HERE? <HACK-TREASURES> <SET HERE? <>>)>
       <DEPOSIT-BOOTY ,TREASURE-ROOM> ;"silent")
      (<AND <EQUAL? .RM ,HERE>
	    <NOT <FSET? .RM ,ONBIT>>
	    <NOT <IN? ,TROLL ,HERE>>>
       <COND (<THIEF-VS-ADVENTURER .HERE?> <RTRUE>)>
       <COND (<FSET? ,THIEF ,INVISIBLE> <SET HERE? <>>)>)
      (T
       <COND (<AND <IN? ,THIEF .RM>
		   <NOT <FSET? ,THIEF ,INVISIBLE>>> ;"Leave if victim left"
	      <FSET ,THIEF ,INVISIBLE>
	      <SET HERE? <>>)>
       <COND (<FSET? .RM ,TOUCHBIT>     ;"Hack the adventurer's belongings"
	      <ROB .RM ,THIEF 75>
	      <SET FLG
		   <COND (<AND <FSET? .RM ,MAZEBIT>
			       <FSET? ,HERE ,MAZEBIT>>
			  <ROB-MAZE .RM>)
			 (T <STEAL-JUNK .RM>)>>)>)>
     <COND (<AND <SET ONCE <NOT .ONCE>> <NOT .HERE?>>
					   ;"Move to next room, and hack."
	    <RECOVER-STILETTO>
	    <REPEAT ()
		    <COND (<AND .RM <SET RM <NEXT? .RM>>>)
			  (T <SET RM <FIRST? ,ROOMS>>)>
		    <COND (<AND <NOT <FSET? .RM ,SACREDBIT>>
				<FSET? .RM ,RLANDBIT>>
			   <MOVE ,THIEF .RM>
			   <FCLEAR ,THIEF ,FIGHTBIT>
			   <FSET ,THIEF ,INVISIBLE>
			   <SETG THIEF-HERE <>>
			   <RETURN>)>>
	    <AGAIN>)>>
   <COND (<NOT <EQUAL? .RM ,TREASURE-ROOM>>
	  <DROP-JUNK .RM>)>
   .FLG>

<ROUTINE DROP-JUNK (RM "AUX" X N (FLG <>))
	 <SET X <FIRST? ,THIEF>>
	 <REPEAT ()
		 <COND (<NOT .X> <RETURN .FLG>)>
		 <SET N <NEXT? .X>>
		 <COND (<EQUAL? .X ,STILETTO ,LARGE-BAG>)
		       (<AND <0? <GETP .X ,P?TVALUE>> <PROB 30 T>>
			<FCLEAR .X ,INVISIBLE>
			<MOVE .X .RM>
			<COND (<AND <NOT .FLG> <EQUAL? .RM ,HERE>>
			       <TELL
"The robber, rummaging through his bag, dropped a few items he found
valueless." CR>
			       <SET FLG T>)>)>
		 <SET X .N>>>

<ROUTINE RECOVER-STILETTO ()
	 <COND (<IN? ,STILETTO <LOC ,THIEF>>
		<FSET ,STILETTO ,NDESCBIT>
		<MOVE ,STILETTO ,THIEF>)>>

<ROUTINE STEAL-JUNK (RM "AUX" X N)
	 <SET X <FIRST? .RM>>
	 <REPEAT ()
		 <COND (<NOT .X> <RFALSE>)>
		 <SET N <NEXT? .X>>
		 <COND (<AND <0? <GETP .X ,P?TVALUE>>
			     <FSET? .X ,TAKEBIT>
			     <NOT <FSET? .X ,SACREDBIT>>
			     <NOT <FSET? .X ,INVISIBLE>>
			     <OR <EQUAL? .X ,STILETTO>
				 <PROB 10 T>>>
			<MOVE .X ,THIEF>
			<FSET .X ,TOUCHBIT>
			<FSET .X ,INVISIBLE>
			<COND (<EQUAL? .X ,ROPE> <SETG DOME-FLAG <>>)>
			<COND (<EQUAL? .RM ,HERE>
			       <TELL "You suddenly notice that the "
				     D .X " vanished." CR>
			       <RTRUE>)
			      (ELSE <RFALSE>)>)>
		 <SET X .N>>>

<ROUTINE ROB (WHAT WHERE "OPTIONAL" (PROB <>) "AUX" N X (ROBBED? <>))
	 <SET X <FIRST? .WHAT>>
	 <REPEAT ()
		 <COND (<NOT .X> <RETURN .ROBBED?>)>
		 <SET N <NEXT? .X>>
		 <COND (<AND <NOT <FSET? .X ,INVISIBLE>>
			     <NOT <FSET? .X ,SACREDBIT>>
			     <G? <GETP .X ,P?TVALUE> 0>
			     <OR <NOT .PROB> <PROB .PROB>>>
			<MOVE .X .WHERE>
			<FSET .X ,TOUCHBIT>
			<COND (<EQUAL? .WHERE ,THIEF> <FSET .X ,INVISIBLE>)>
			<SET ROBBED? T>)>
		 <SET X .N>>>

;"special-cased routines"

<ROUTINE V-DIAGNOSE ("AUX" (MS <FIGHT-STRENGTH <>>)
		     (WD <GETP ,WINNER ,P?STRENGTH>) (RS <+ .MS .WD>))
	 #DECL ((MS WD RS) FIX)
	 <COND (<0? <GET <INT I-CURE> ,C-ENABLED?>> <SET WD 0>)
	       (ELSE <SET WD <- .WD>>)>
	 <COND (<0? .WD> <TELL "You are in perfect health.">)
	       (T
		<TELL "You have ">
		<COND (<1? .WD> <TELL "a light wound,">)
		      (<EQUAL? .WD 2> <TELL "a serious wound,">)
		      (<EQUAL? .WD 3> <TELL "several wounds,">)
		      (<G? .WD 3> <TELL "serious wounds,">)>)>
	 <COND (<NOT <0? .WD>>
		<TELL " which will be cured after ">
		<PRINTN
		 <+ <* ,CURE-WAIT <- .WD 1>>
		    <GET <INT I-CURE> ,C-TICK>>>
		<TELL " moves.">)>
	 <CRLF>
	 <TELL "You can ">
	 <COND (<0? .RS> <TELL "expect death soon">)
	       (<1? .RS> <TELL "be killed by one more light wound">)
	       (<EQUAL? .RS 2> <TELL "be killed by a serious wound">)
	       (<EQUAL? .RS 3> <TELL "survive one serious wound">)
	       (<G? .RS 3>
		<TELL "survive several wounds">)>
	 <TELL "." CR>
	 <COND (<NOT <0? ,DEATHS>>
		<TELL "You have been killed ">
		<COND (<1? ,DEATHS> <TELL "once">)
		      (T <TELL "twice">)>
		<TELL "." CR>)>>

<ROUTINE V-SCORE ("OPTIONAL" (ASK? T))
	 #DECL ((ASK?) <OR ATOM FALSE>)
	 <TELL "Your score is ">
	 <TELL N ,SCORE>
	 <TELL " (total of 350 points), in ">
	 <TELL N ,MOVES>
	 <COND (<1? ,MOVES> <TELL " move.">) (ELSE <TELL " moves.">)>
	 <CRLF>
	 <TELL "This gives you the rank of ">
	 <COND (<EQUAL? ,SCORE 350> <TELL "Master Adventurer">)
	       (<G? ,SCORE 330> <TELL "Wizard">)
	       (<G? ,SCORE 300> <TELL "Master">)
	       (<G? ,SCORE 200> <TELL "Adventurer">)
	       (<G? ,SCORE 100> <TELL "Junior Adventurer">)
	       (<G? ,SCORE 50> <TELL "Novice Adventurer">)
	       (<G? ,SCORE 25> <TELL "Amateur Adventurer">)
	       (T <TELL "Beginner">)>
	 <TELL "." CR>
	 ,SCORE>

<ROUTINE JIGS-UP (DESC "OPTIONAL" (PLAYER? <>))
 	 <SETG WINNER ,ADVENTURER>
	 <COND (,DEAD
		<TELL "|
It takes a talented person to be killed while already dead. YOU are such
a talent. Unfortunately, it takes a talented person to deal with it.
I am not such a talent. Sorry." CR>
		<FINISH>)>
	 <TELL .DESC CR>
	 <COND (<NOT ,LUCKY>
		<TELL "Bad luck, huh?" CR>)>
	 <PROG ()
	       <SCORE-UPD -10>
	       <TELL "
|    ****  You have died  ****
|
|">
	       <COND (<FSET? <LOC ,WINNER> ,VEHBIT>
		      <MOVE ,WINNER ,HERE>)>
	       <COND
		(<NOT <L? ,DEATHS 2>>
		 <TELL
"You clearly are a suicidal maniac.  We don't allow psychotics in the
cave, since they may harm other adventurers.  Your remains will be
installed in the Land of the Living Dead, where your fellow
adventurers may gloat over them." CR>
		 <FINISH>)
		(T
		 <SETG DEATHS <+ ,DEATHS 1>>
		 <MOVE ,WINNER ,HERE>
		 <COND (<FSET? ,SOUTH-TEMPLE ,TOUCHBIT>
			<TELL
"As you take your last breath, you feel relieved of your burdens. The
feeling passes as you find yourself before the gates of Hell, where
the spirits jeer at you and deny you entry.  Your senses are
disturbed.  The objects in the dungeon appear indistinct, bleached of
color, even unreal." CR CR>
			<SETG DEAD T>
			<SETG TROLL-FLAG T>
			;<SETG GWIM-DISABLE T>
			<SETG ALWAYS-LIT T>
			<PUTP ,WINNER ,P?ACTION DEAD-FUNCTION>
			<GOTO ,ENTRANCE-TO-HADES>)
		       (T
			<TELL
"Now, let's take a look here...
Well, you probably deserve another chance.  I can't quite fix you
up completely, but you can't have everything." CR CR>
			<GOTO ,FOREST-1>)>
		 <FCLEAR ,TRAP-DOOR ,TOUCHBIT>
		 <SETG P-CONT <>>
		 <RANDOMIZE-OBJECTS>
		 <KILL-INTERRUPTS>
		 <RFATAL>)>>>

<ROUTINE RANDOMIZE-OBJECTS ("AUX" (R <>) F N L)
	 <COND (<IN? ,LAMP ,WINNER>
		<MOVE ,LAMP ,LIVING-ROOM>)>
	 <COND (<IN? ,COFFIN ,WINNER>
		<MOVE ,COFFIN ,EGYPT-ROOM>)>
	 <PUTP ,SWORD ,P?TVALUE 0>
	 <SET N <FIRST? ,WINNER>>
	 <SET L <GET ,ABOVE-GROUND 0>>
	 <REPEAT ()
		 <SET F .N>
		 <COND (<NOT .F> <RETURN>)>
		 <SET N <NEXT? .F>>
		 <COND (<G? <GETP .F ,P?TVALUE> 0>
			<REPEAT ()
				<COND (<NOT .R> <SET R <FIRST? ,ROOMS>>)>
				<COND (<AND <FSET? .R ,RLANDBIT>
					    <NOT <FSET? .R ,ONBIT>>
					    <PROB 50>>
				       <MOVE .F .R>
				       <RETURN>)
				      (ELSE <SET R <NEXT? .R>>)>>)
		       (ELSE
			<MOVE .F <GET ,ABOVE-GROUND <RANDOM .L>>>)>>>

<ROUTINE KILL-INTERRUPTS ()
	 <DISABLE <INT I-XB>>
	 <DISABLE <INT I-XC>>
	 <DISABLE <INT I-CYCLOPS>>
	 <DISABLE <INT I-LANTERN>>
	 <DISABLE <INT I-CANDLES>>
	 <DISABLE <INT I-SWORD>>
	 <DISABLE <INT I-FOREST-ROOM>>
	 <DISABLE <INT I-MATCH>>
	 <FCLEAR ,MATCH ,ONBIT>
	 <RTRUE>>

<ROUTINE BAG-OF-COINS-F ()
	 <STUPID-CONTAINER ,BAG-OF-COINS "coins">>

<ROUTINE TRUNK-F ()
	 <STUPID-CONTAINER ,TRUNK "jewels">>

<ROUTINE STUPID-CONTAINER (OBJ STR)
	 <COND (<VERB? OPEN CLOSE>
		<TELL
"The " .STR " are safely inside; there's no need to do that." CR>)
	       (<VERB? LOOK-INSIDE EXAMINE>
		<TELL
"There are lots of " .STR " in there." CR>)
	       (<AND <VERB? PUT> <EQUAL? ,PRSI .OBJ>>
		<TELL
"Don't be silly. It wouldn't be a " D .OBJ " anymore." CR>)>>

<ROUTINE DUMB-CONTAINER ()
	 <COND (<VERB? OPEN CLOSE LOOK-INSIDE>
		<TELL "You can't do that." CR>)
	       (<VERB? EXAMINE>
		<TELL "It looks pretty much like a " D ,PRSO "." CR>)>>

<ROUTINE GARLIC-F ()
	 <COND (<VERB? EAT>
		<REMOVE-CAREFULLY ,PRSO>
		<TELL
"What the heck! You won't make friends this way, but nobody around
here is too friendly anyhow. Gulp!" CR>)>>

<ROUTINE CHAIN-PSEUDO ()
	 <COND (<VERB? TAKE MOVE>
		<TELL "The chain is secure." CR>)
	       (<VERB? RAISE LOWER>
		<TELL "Perhaps you should do that to the basket." CR>)
	       (<VERB? EXAMINE>
		<TELL "The chain secures a basket within the shaft." CR>)>>

<ROUTINE TROLL-ROOM-F (RARG)
	 <COND (<AND <EQUAL? .RARG ,M-ENTER>
		     <IN? ,TROLL ,HERE>>
		<THIS-IS-IT ,TROLL>)>>