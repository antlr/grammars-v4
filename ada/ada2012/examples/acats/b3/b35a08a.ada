-- B35A08A.ADA

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
--     CHECK THAT THE MULTIPLICATION AND DIVISION OPERATORS FOR ONE
--     FIXED POINT OPERAND AND ONE INTEGER OPERAND ARE NOT DIRECTLY
--     VISIBLE.

-- HISTORY:
--     BCB 01/21/88  CREATED ORIGINAL TEST.

PROCEDURE B35A08A IS

     PACKAGE P IS
          TYPE T IS DELTA 0.25 RANGE -100.0 .. 100.0;
     END P;

     X1 : P.T := 1.0;
     X2 : P.T := 2.0;
     X3 : P.T := 2 * X1;                               -- ERROR:
     X4 : P.T := X2 * 3;                               -- ERROR:
     X5 : P.T := X1 / 2;                               -- ERROR:

BEGIN
     NULL;
END B35A08A;
