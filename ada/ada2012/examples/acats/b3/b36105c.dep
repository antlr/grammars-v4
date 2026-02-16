-- B36105C.DEP

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
--     WHEN L AND R ARE BOTH IN THE RANGE OF SHORT_INTEGER VALUES,
--     CHECK THAT NEITHER A LOOP PARAMETER NOR AN ARRAY INDEX OF THE
--     FORM L .. R IS ASSUMED TO HAVE THE TYPE SHORT_INTEGER.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO THOSE IMPLEMENTATIONS WHICH
--     SUPPORT SHORT_INTEGER.

--     IF SHORT_INTEGER IS NOT SUPPORTED, THEN THE DECLARATION OF
--     "CHECK_SHORT" MUST BE REJECTED.

-- HISTORY:
--     BCB 07/11/88  CREATED ORIGINAL TEST.

PROCEDURE B36105C IS

     CHECK_SHORT : SHORT_INTEGER := 2;                 -- N/A => ERROR.

     TYPE A IS ARRAY(SHORT_INTEGER RANGE <>) OF INTEGER;

     TYPE C IS ARRAY(1..10) OF INTEGER;

     B : A(1..10);

     D : C;

BEGIN

     FOR I IN 1 .. 10 LOOP
          CHECK_SHORT := I;                            -- ERROR:
     END LOOP;

     FOR J IN 1 .. 10 LOOP
          B (J) := 10;                                 -- ERROR:
     END LOOP;

     D (CHECK_SHORT) := 15;                            -- ERROR:

END B36105C;
