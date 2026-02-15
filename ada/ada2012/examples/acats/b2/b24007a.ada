-- B24007A.ADA

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
-- CHECK THAT REAL LITERALS MUST HAVE POINTS
-- (BOTH BASED LITERALS AND DECIMAL LITERALS).

-- PWB  2/13/86

PROCEDURE B24007A IS

     TYPE FLOATING IS DIGITS 5;                        -- O.K.
     TYPE FIXED    IS DELTA 0.1 
                   RANGE -10.0 .. 10.0;                -- O.K.

     TYPE TERR1 IS DELTA 1 RANGE -100.0 .. 100.0;      -- ERROR: 1
     TYPE TERR2 IS DELTA 0.1 RANGE -100 .. 0.0;        -- ERROR: 100
     TYPE TERR3 IS DELTA 0.1 RANGE -100.0 .. 0;        -- ERROR: 0
     TYPE TERR4 IS DELTA 2#10#                         -- ERROR: 10
                   RANGE 2#0.0# .. 2#10.0#;
     TYPE TERR5 IS DELTA 2#0.1#
                   RANGE 2#0# .. 2#1000.0#;            -- ERROR: 0
     TYPE TERR6 IS DELTA 2#10#E-2                      -- ERROR: 10
                   RANGE 2#0.0# .. 2#10.0#E3;
     SUBTYPE TERR7 IS FLOATING RANGE 0.0 .. 100;       -- ERROR: 100
     SUBTYPE TERR8 IS FLOATING RANGE 0 .. 10.0;        -- ERROR: 0

     IERR1 : FLOATING := 32;                           -- ERROR: 32
     IERR2 : FIXED    := 3#12#E-2;                     -- ERROR: 12
     IERR3 : FLOATING := 3#121#;                       -- ERROR: 121

     FLOAT1, FLOAT2 : FLOATING := 0.0;
     FIXED1, FIXED2 : FIXED := 0.0;

BEGIN

     FLOAT1 := 14;                                     -- ERROR: 14
     FIXED1 := 13E2;                                   -- ERROR: 13
     FLOAT2 := 8#12#E-3;                               -- ERROR: 12
     DELAY 2E-5;                                       -- ERROR: 2
     IF FLOAT1 = 14 THEN                               -- ERROR: 14
          NULL; 
     END IF;
     IF FLOAT2 > 8#12#E-2 THEN                         -- ERROR: 12
          FLOAT2 := FLOAT1 * 14E2;                     -- ERROR: 14
     END IF;
     FIXED2 := FIXED1 + 2#101101#E-1;                  -- ERROR: 101101

END B24007A;
