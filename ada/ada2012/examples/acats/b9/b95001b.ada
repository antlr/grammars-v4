-- B95001B.ADA

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
-- CHECK THAT THE BASE TYPE OF AN INDEX IN AN ENTRY CALL OR AN ACCEPT 
-- STATEMENT MUST MATCH THAT GIVEN IN THE ENTRY FAMILY'S DECLARATION.

-- RM 1/15/82
-- R.WILLIAMS 9/15/86  -- ADDED ACCESS TO TASK TYPE TO CHECK INDEX OF
--                        ENTRY FAMILY. RENAMED FROM -AB. REVISED 
--                        COMMENTS TO CONFORM THOSE IN AIG.

PROCEDURE B95001B IS

     SUBTYPE S IS INTEGER RANGE 5 .. 50;
     TYPE S1 IS NEW S;
     X : S1 := 6;
     Y : S  := 6;
     TYPE U IS (EN1, EN2, EN3, EN4);
     TYPE NBOOL IS NEW BOOLEAN;
     NTRUE : NBOOL := TRUE;
     TYPE CHAR IS ('A', 'B', 'C');
     A : CHAR := 'A';

     TASK TK IS
	 ENTRY E1 (S1 RANGE 5 .. 10);
	 ENTRY E2 (INTEGER RANGE 0 .. 20) (I : IN INTEGER);
	 ENTRY E3 (U RANGE EN1 .. EN4);
     END;

     TASK TYPE TT IS
          ENTRY F1 (S1 RANGE 5 .. 10);
          ENTRY F2 (INTEGER RANGE 5 .. 10);
          ENTRY F3 (CHARACTER RANGE 'A' .. 'Z');
          ENTRY F4 (BOOLEAN);
          ENTRY F5 (U);
     END TT;

     TYPE TT_NAME IS ACCESS TT;
     TTN : TT_NAME;

     TASK BODY TK IS
     BEGIN
	  ACCEPT E1 (Y);        -- ERROR: DIFFERENT INDEX BASE TYPES.
	  NULL;
	  ACCEPT E1 (EN2);      -- ERROR: DIFFERENT INDEX BASE TYPES.
	  NULL;
	  ACCEPT E1 (FALSE);    -- ERROR: DIFFERENT INDEX BASE TYPES.
	  NULL;
	  ACCEPT E1 (ASCII.CR); -- ERROR: DIFFERENT INDEX BASE TYPES.
	  NULL;
	  ACCEPT E2 (X)         -- ERROR: DIFFERENT INDEX BASE TYPES.
	       (I : IN INTEGER);
	  NULL;
	  ACCEPT E3 (3);        -- ERROR: DIFFERENT INDEX BASE TYPES.
     END;

     TASK BODY TT IS
     BEGIN
          ACCEPT F1 (Y);        -- ERROR: DIFFERENT INDEX BASE TYPES.
          NULL;
          ACCEPT F2 (X);        -- ERROR: DIFFERENT INDEX BASE TYPES.
          NULL;
          ACCEPT F3 (A);        -- ERROR: DIFFERENT INDEX BASE TYPES.
          NULL;
          ACCEPT F4 (NTRUE);    -- ERROR: DIFFERENT INDEX BASE TYPES.
          NULL;
          ACCEPT F5 
               (U'POS (EN1));   -- ERROR: DIFFERENT INDEX BASE TYPES.
     END TT;

BEGIN

     TK.E1 (Y);                 -- ERROR: DIFFERENT INDEX BASE TYPES.
     TK.E1 (EN2);               -- ERROR: DIFFERENT INDEX BASE TYPES.
     TK.E1 (FALSE);             -- ERROR: DIFFERENT INDEX BASE TYPES.
     TK.E1 (ASCII.CR);          -- ERROR: DIFFERENT INDEX BASE TYPES.
     TK.E2 (X) (17);            -- ERROR: DIFFERENT INDEX BASE TYPES.
     TK.E3 (3);                 -- ERROR: DIFFERENT INDEX BASE TYPES.

     TTN := NEW TT;

     TTN.F1 (Y);                -- ERROR: DIFFERENT INDEX BASE TYPES.
     TTN.F2 (X);                -- ERROR: DIFFERENT INDEX BASE TYPES.
     TTN.F3 (A);                -- ERROR: DIFFERENT INDEX BASE TYPES.
     TTN.F4 (NTRUE);            -- ERROR: DIFFERENT INDEX BASE TYPES.
     TTN.F5 (U'POS (EN1));      -- ERROR: DIFFERENT INDEX BASE TYPES.

END B95001B;
