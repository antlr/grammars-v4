-- B37104A.ADA

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
-- CHECK THAT A DISCRIMINANT'S DEFAULT INITIAL VALUE MUST HAVE THE
-- SAME TYPE AS THE DISCRIMINANT.

-- RJW 2/27/86 

PROCEDURE B37104A IS
     
     TYPE CHAR1 IS NEW CHARACTER;
     TYPE CHAR2 IS NEW CHARACTER;
     
     C1 : CHAR1 := 'A';
     C2 : CHAR2 := 'A';

     TYPE REC1 (DISC1 : CHAR1 := C2;        -- ERROR: DIFFERENT TYPES.
                DISC2 : CHAR2 := C1) IS     -- ERROR: DIFFERENT TYPES.
          RECORD
               NULL;
          END RECORD;
     
     TYPE INT IS RANGE 1 .. 10;
     I : INT := 1;

     TYPE REC2 (DISC : INTEGER := I) IS     -- ERROR: DIFFERENT TYPES.
          RECORD
               NULL;
          END RECORD;
     
     TYPE TBOOL IS NEW BOOLEAN;
     
     TYPE REC3 (T : TBOOL := BOOLEAN'(FALSE)) IS  -- ERROR: DIFFERENT 
                                                  --        TYPES.
          RECORD
               NULL;
          END RECORD;

     TYPE ENUM1 IS (A, B, C);
     TYPE ENUM2 IS (C, D, E);

     E1 : ENUM1 := A;
     E2 : ENUM2 := E;

     TYPE REC4 (DISC1 : ENUM1 := E2;        -- ERROR: DIFFERENT TYPES.
                DISC2 : ENUM2 := E1) IS     -- ERROR: DIFFERENT TYPES.
          RECORD
               NULL;
          END RECORD;
BEGIN
     NULL;          
END B37104A;                                
