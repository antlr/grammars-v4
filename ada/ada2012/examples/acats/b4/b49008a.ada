-- B49008A.ADA

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
--     CHECK THAT A STATIC EXPRESSION CANNOT CONTAIN A CALL TO A
--     USER-DEFINED OPERATOR.

-- HISTORY:
--     LB  08/27/86 CREATED ORIGINAL TEST.
--     RJW 03/28/90 REVISED TEST TO CONFORM TO AI-00438.

PROCEDURE  B49008A  IS

     TYPE INT IS RANGE 1 .. 25;
     TYPE REAL IS DIGITS 5;
     FUNCTION "&"(X,Y : INT) RETURN INT IS
          BEGIN
               RETURN X + Y;
          END "&";
     FUNCTION "/"(X,Y : REAL) RETURN REAL IS
          BEGIN
               RETURN X - Y;
          END "/";
BEGIN
     DECLARE
          TYPE INT1 IS RANGE 1 .. 2&5;                         -- ERROR:
          CAS_OBJ : INT := 7;
          TYPE FIX IS DELTA 3.0*(REAL'(5.0)/REAL'(4.0))        -- ERROR:
                         RANGE 0.0 .. 10.0;
     BEGIN
          NULL;
     END;

END B49008A;
