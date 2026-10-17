-- B37409B.ADA

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
-- OBJECTIVE:
--     CHECK THAT THE ATTRIBUTE 'BASE IS NOT DEFINED FOR OBJECTS OF
--     RECORD TYPES AND SUBTYPES.

-- HISTORY:
--     DHH 03/03/88 CREATED ORIGINAL TEST.

PROCEDURE B37409B IS
     SUBTYPE INT IS INTEGER RANGE 1 .. 10;
     TYPE T(A : INT := 1) IS
          RECORD
               I : INTEGER;
          END RECORD;
     TYPE TA IS
          RECORD
               I : INTEGER;
          END RECORD;
     SUBTYPE SS IS T(6);
     X : SS;
     Y : TA;
BEGIN

     IF X'BASE'SIZE < T'SIZE THEN         -- ERROR: 'BASE APPLIED TO
                                          --         RECORD OBJECT.
          X.I := 1;
     END IF;
     IF Y'BASE'SIZE < T'SIZE THEN         -- ERROR: 'BASE APPLIED TO
                                          --         RECORD OBJECT.
          Y.I := 1;
     END IF;

END B37409B;
