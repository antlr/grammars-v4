-- B84007A.ADA

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
--     CHECK THAT THE NAMES MADE VISIBLE BY A USE CLAUSE ARE NOT MADE
--     VISIBLE UNTIL AFTER THE END OF THE USE CLAUSE.

-- HISTORY:
--     JET 03/10/88 CREATED ORIGINAL TEST.

PROCEDURE B84007A IS

     PACKAGE PACK1 IS
          PACKAGE PACK2 IS
               TYPE A IS RANGE 0..100;
          END PACK2;
     END PACK1;

     USE PACK2;                    -- ERROR: PACK2 NOT VISIBLE.

     X : INTEGER;

     USE PACK1, PACK2;             -- ERROR: PACK2 STILL NOT VISIBLE.

BEGIN
     NULL;
END B84007A;
