-- B85001G.ADA

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
--     CHECK THAT AN OBJECT RENAMING DECLARATION CANNOT RENAME AN ATTRIBUTE 
--     WHICH RETURNS A VALUE.

-- HISTORY:
--     JET 07/21/88  CREATED ORIGINAL TEST.
--     TBN 10/11/90  REVISED THE COMPONENT REC.I TO BE OF TYPE REC0.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

PROCEDURE B85001G IS
     TYPE REC0 IS RECORD
          F : INTEGER;
     END RECORD;

     TYPE REC IS RECORD
          I : REC0;
          B : BOOLEAN;
     END RECORD;

     FUNCTION FUNK RETURN REC;

     A : REC := (I => (F=> 0), B => FALSE);
     B : REC0 RENAMES A.I;              -- OK.

     U : INTEGER RENAMES INTEGER'FIRST; -- ERROR: RENAMES ATTRIBUTE.

     FUNCTION FUNK RETURN REC IS
     BEGIN
          RETURN (I => (F => 0), B => TRUE);
     END FUNK;

BEGIN
     NULL;
END B85001G;
