-- B85008F.ADA

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
--     VARIABLE, CONSTANT, OR SUBCOMPONENT.

-- HISTORY:
--     JET 07/25/88  CREATED ORIGINAL TEST.

PROCEDURE B85008F IS

     TYPE REC0 IS RECORD
          G : INTEGER;
     END RECORD;

     TYPE REC IS RECORD
          F : REC0;
     END RECORD;

     V : INTEGER;
     C1 : CONSTANT := 1;
     C2 : CONSTANT INTEGER := 2;
     R : REC;

     RV : EXCEPTION RENAMES V;               -- ERROR: VARIABLE.
     RC1 : EXCEPTION RENAMES C1;             -- ERROR: NAMED NUMBER.
     RC2 : EXCEPTION RENAMES C2;             -- ERROR: CONSTANT.
     RR : EXCEPTION RENAMES R.F.G;           -- ERROR: SUBCOMPONENT.

     PROCEDURE PROC (P : IN INTEGER) IS
          RP : EXCEPTION RENAMES P;          -- ERROR: FORMAL PARAMETER.
     BEGIN
          NULL;
     END PROC;

BEGIN
     FOR L IN 1..10 LOOP
          DECLARE
               RL : EXCEPTION RENAMES L;     -- ERROR: LOOP VARIABLE.
          BEGIN
               NULL;
          END;
     END LOOP;
END B85008F;
