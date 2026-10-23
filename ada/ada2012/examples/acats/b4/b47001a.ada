-- B47001A.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- CHECK THAT THE OPERAND OF A QUALIFIED EXPRESSION MUST HAVE THE SAME
-- BASE TYPE AS THE TYPE MARK.

-- RJW 7/22/86

PROCEDURE B47001A IS
     
     TYPE DAY IS (SUN, MON, TUES, WED, THURS, FRI, SAT);
     SUBTYPE WEEKDAY IS DAY RANGE MON .. FRI;

     TYPE NEWDAY1 IS NEW DAY;
     TYPE NEWDAY2 IS NEW DAY;

     D : DAY := SUN;
     ND1 : NEWDAY1 := SUN;
     ND2 : NEWDAY2 := SUN;

     I5 : INTEGER := 5;

     TYPE INT IS RANGE -10 .. 10;
     I1 : INT := 1;

     TYPE FIXED IS DELTA 0.5 RANGE -5.0 .. 5.0;
     SUBTYPE SFIXED IS FIXED RANGE 0.0 .. 1.0;
     FI : FIXED := -1.0;

     TYPE FLT IS DIGITS 3 RANGE -5.0 .. 5.0;
     SUBTYPE SFLT IS FLT RANGE -1.0 .. 0.0;
     FL : FLT := 1.0;

     TYPE CARR1 IS ARRAY (1 .. 10) OF INTEGER;
     C1 : CARR1 := (OTHERS => 0);

     TYPE CARR2 IS ARRAY (11 .. 20) OF INTEGER;
     C2 : CARR2 := (OTHERS => 0);

     TYPE ARR1 IS ARRAY (NATURAL RANGE <>) OF INTEGER;
     SUBTYPE SARR1A IS ARR1 (1 .. 10);
     
     SA1 : SARR1A := (OTHERS => 0);

     TYPE ARR2 IS ARRAY (NATURAL RANGE <>) OF INTEGER;     
     AR2 : ARR2 (21 .. 30) := (OTHERS => 0);

BEGIN
     D := WEEKDAY'(D);                     -- OK: QUALIFIED.
     
     ND1 := NEWDAY1 (D);                   -- OK: CONVERSION.
     ND1 := NEWDAY1'(D);                   -- ERROR: QUALIFIED.

     ND2 := NEWDAY2 (ND1);                 -- OK: CONVERSION.
     ND2 := NEWDAY2'(ND1);                 -- ERROR: QUALIFIED.

     I1 := INT (I5);                       -- OK: CONVERSION.
     I1 := INT'(I5);                       -- ERROR: QUALIFIED.

     FI := SFIXED'(FI);                    -- OK: QUALIFIED.

     FI := SFIXED (1);                     -- OK: CONVERSION.
     FI := SFIXED'(1);                     -- ERROR: QUALIFIED.    

     FI := SFIXED (I1);                    -- OK: CONVERSION.
     FI := SFIXED'(I1);                    -- ERROR: QUALIFIED.

     FI := FIXED (FL);                     -- OK: CONVERSION.
     FI := FIXED'(FL);                     -- ERROR: QUALIFIED.

     FL := SFLT'(0.0);                     -- OK: QUALIFIED.
     
     FL := FLT (-1);                       -- OK: CONVERSION.
     FL := FLT'(-1);                       -- ERROR: QUALIFIED.
     
     FL := FLT (I5);                       -- OK: CONVERSION.
     FL := FLT'(I5);                       -- ERROR: QUALIFIED.

     C1 := CARR1 (C2);                     -- OK: CONVERSION.
     C1 := CARR1'(C2);                     -- ERROR: QUALIFIED.

     C2 := CARR2 (SA1);                    -- OK: CONVERSION.
     C2 := CARR2'(SA1);                    -- ERROR: QUALIFIED.

     SA1 := ARR1 (AR2);                    -- OK: CONVERSION.
     SA1 := ARR1'(AR2);                    -- ERROR: QUALIFIED.

     AR2 := ARR2 (C1);                    -- OK: CONVERSION.
     AR2 := ARR2'(C1);                    -- ERROR: QUALIFIED.

END B47001A;
