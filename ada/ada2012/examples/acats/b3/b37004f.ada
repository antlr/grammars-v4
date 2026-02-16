-- B37004F.ADA

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
-- CHECK THAT VACUOUS CASE TYPE DECLARATIONS ARE FORBIDDEN.

-- DAT 5/18/81
-- ABW 6/11/82
-- SPS 2/10/83

PROCEDURE B37004F IS

     SUBTYPE S1 IS INTEGER RANGE 1 .. 1;

     TYPE R1 (D : S1) IS RECORD
          CASE D IS
               WHEN 1 => NULL;
          END CASE;
     END RECORD;                        -- OK.

     SUBTYPE NULL_BOOL IS BOOLEAN RANGE TRUE .. FALSE;
     TYPE REC (D : NULL_BOOL) IS RECORD
          CASE D IS END CASE;           -- ERROR: MISSING CHOICE.
     END RECORD;

BEGIN
     NULL;

END B37004F;
