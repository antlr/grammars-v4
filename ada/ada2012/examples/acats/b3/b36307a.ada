-- B36307A.ADA

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
-- CHECK THAT DECLARATION OF A MULTIDIMENSIONAL ARRAY OF CHARACTER TYPE
-- DOES NOT IMPLICITLY DECLARE A STRING LITERAL FORMATION OPERATION.

-- L.BROWN   08/18/86

PROCEDURE B36307A IS

     -- CHECK THAT NO STRING LITERAL FORMATION OPERATION IS DECLARED
     -- FOR A MULTIDIMENSIONAL ARRAY TYPE.

     TYPE ARR_TYPE IS ARRAY( 1 .. 2, 1 .. 4) OF CHARACTER;

     AR1 : ARR_TYPE := "ABCDEFGH";               -- ERROR: STRING LIT.
     AR2 : ARR_TYPE;
     AR3 : ARR_TYPE;

     GENERIC
          AR4 : ARR_TYPE;
     PROCEDURE PROC;
     PROCEDURE PROC IS
     BEGIN
          NULL;
     END PROC;

     PROCEDURE PROC1 IS NEW PROC("ADSGFDJA");    -- ERROR: STRING LIT.

     FUNCTION FUN (ARR : ARR_TYPE) RETURN ARR_TYPE IS
          BEGIN
               RETURN ARR;
          END FUN;

BEGIN

     AR2 := "MNOPQRSG";                          -- ERROR: STRING LIT.
     AR3 := FUN("AGDEKGFB");                     -- ERROR: STRING LIT.

     IF AR1 = "ABCDEFGH"  THEN                   -- ERROR: STRING LIT.
          AR1 := ("ADFS","DJFI");
     END IF;

END B36307A;
