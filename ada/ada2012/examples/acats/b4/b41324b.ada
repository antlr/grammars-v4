-- B41324B.ADA

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
-- CHECK THAT THE OPERATIONS FOR MULTIPLYING AND DIVIDING TWO FIXED
-- POINT OPERANDS ARE NOT IMPLICITLY DECLARED IN THE PACKAGE WHERE THE
-- TYPE IS DECLARED.  CHECK THAT THE FIRST OPERAND FOR DIVISION CANNOT
-- BE AN INTEGER.

-- TBN  7/17/86

PROCEDURE B41324B IS

     PACKAGE P IS
          TYPE FIXED IS DELTA 0.125 RANGE -1.0E1 .. 1.0E1;
          OBJ_FIX_2 : FIXED := 1.5;
     END P;

     FIX_VAR_1 : P.FIXED := P."-" (P.FIXED (9.0));
     FIX_VAR_2 : P.FIXED := 1.5;
     FIX_VAR_3 : P.FIXED := 1.0E1;

BEGIN

     FIX_VAR_1 := P."*" (FIX_VAR_2, P.FIXED (2.1));            -- ERROR:
     NULL;
     FIX_VAR_1 := P."*" (P.OBJ_FIX_2, P.FIXED (2.1));          -- ERROR:
     NULL;
     FIX_VAR_1 := P."*" (3, FIX_VAR_2);                        -- OK.

     FIX_VAR_1 := P."/" (FIX_VAR_3, P.OBJ_FIX_2);              -- ERROR:
     NULL;
     FIX_VAR_1 := P."/" (FIX_VAR_3, FIX_VAR_2);                -- ERROR:
     NULL;
     FIX_VAR_1 := P."/" (FIX_VAR_3, 2);                        -- OK.

     FIX_VAR_1 := P."/" (2, P.FIXED (10.0));                   -- ERROR:
     NULL;
     FIX_VAR_1 := P."/" (2, FIX_VAR_3);                        -- ERROR:

END B41324B;
