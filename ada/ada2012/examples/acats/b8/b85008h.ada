-- B85008H.ADA

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
--     CHECK THAT AN EXCEPTION RENAMING DECLARATION CANNOT RENAME A
--     SUBCOMPONENT OF A FUNCTION VALUE WHICH RETURNS A VALUE.

-- HISTORY:
--     JET 07/25/88  CREATED ORIGINAL TEST.
--     THS 04/12/90  SPLIT TEST TO B85008J.ADA.

PROCEDURE B85008H IS

     TYPE REC0 IS RECORD
          J : INTEGER;
     END RECORD;

     TYPE REC IS RECORD
          I : REC0;
          B : BOOLEAN;
     END RECORD;

     FUNCTION FUNK RETURN REC;

     A : REC := ((J => 0), FALSE);
     T : EXCEPTION RENAMES FUNK.I.J;      -- ERROR: RENAMES SUBCOMPONENT
                                          -- OF FUNCTION VALUE.

     FUNCTION FUNK RETURN REC IS
     BEGIN
          RETURN ((J => 1), TRUE);
     END FUNK;

BEGIN
     NULL;
END B85008H;
