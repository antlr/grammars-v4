-- B25002B.ADA

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
-- CHECK THAT THE FORMAT EFFECTORS CANNOT
-- APPEAR IN CHARACTER LITERALS:
--     HORIZONTAL TAB,
--     VERTICAL TAB,
--     CARRIAGE RETURN,
--     LINE FEED,
--     FORM FEED.

-- PWB  2/14/86

PROCEDURE B25002B IS

     LINE : STRING (1..3);
     CHAR : CHARACTER;

     HT : CONSTANT CHARACTER := '	';    -- ERROR: HT.
     C1 : CHARACTER;  -- TO AID RECOVERY
                                              -- ERROR: VT NEXT LINE.
     VT : CONSTANT CHARACTER := '';
     C2 : CHARACTER;  -- TO AID RECOVERY
                                              -- ERROR: CR NEXT LINE;
     CR : CHARACTER := '
';
     C3 : CHARACTER;  -- TO AID RECOVERY

     BAD_LINE : STRING(1..3) := "LF" &        -- ERROR: LF NEXT LINE.
                               '
';

     C4 : CHARACTER;  -- TO AID RECOVERY
     FORM : STRING(1..3) :=                  -- ERROR: FF NEXT LINE.
                           '
' & "FF";
     C5 : CHARACTER;  -- TO AID RECOVERY

BEGIN

     CHAR := '	';                            -- ERROR: HT.
     CHAR := 'A';  -- TO AID RECOVERY

     LINE := LINE(1..2) &                     -- ERROR: VT NEXT LINE.
             '';
     CHAR := 'A';  -- TO AID RECOVERY

                                              -- ERROR: CR NEXT LINE.
     LINE := '
' 
             & LINE(2..3);
     CHAR := 'A';  -- TO AID RECOVERY

     LINE := LINE(1..1) &                     -- ERROR: FF NEXT LINE.
             '
' 
             & LINE(3..3);
     CHAR := 'A';  -- TO AID RECOVERY

     CHAR :=                                  -- ERROR: LF NEXT LINE.
             '
';
     CHAR := 'A';  -- TO AID RECOVERY

END B25002B;
