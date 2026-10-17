-- B84008B.ADA

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
--     CHECK THAT THE NAMES MADE VISIBLE BY A USE CLAUSE IN THE VISIBLE
--     PART OF A PACKAGE ARE NOT MADE VISIBLE OUTSIDE THE PACKAGE EVEN
--     IF A USE CLAUSE IS GIVEN FOR THE PACKAGE IN WHICH THE ORIGINAL
--     USE CLAUSE IS GIVEN.

-- HISTORY:
--     JET 07/21/88  CREATED ORIGINAL TEST.

PROCEDURE B84008B IS

     PACKAGE PACK1 IS
          TYPE A IS RANGE 0..100;
          PROCEDURE PROC;
          I : INTEGER;
     END PACK1;

     PACKAGE PACK2 IS
          USE PACK1;
          VAR1 : A;
     END PACK2;

     USE PACK2;

     VAR2 : A;                         -- ERROR: A NOT VISIBLE.

     PACKAGE BODY PACK1 IS
          PROCEDURE PROC IS
          BEGIN
               NULL;
          END PROC;
     END PACK1;

BEGIN
     PROC;                             -- ERROR: PROC NOT VISIBLE.
     I := 10;                          -- ERROR: I NOT VISIBLE.
END B84008B;
