-- B95082E.ADA

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
-- FOR ENTRIES HAVING AT LEAST ONE DEFAULT PARAMETER,
-- CHECK THAT:
--        (A) CALLS OF THE FORM T.F(A,,C) ARE FORBIDDEN, WHERE THE
--            SECOND FORMAL PARAMETER HAS A DEFAULT VALUE.
--        (B) FOR A CALL USING ONLY POSITIONAL NOTATION, NO
--            PARAMETER CAN BE OMITTED UNLESS THE DEFAULT
--            PARAMETERS ARE AT THE END OF THE LIST.
--        (C) FOR A CALL USING NAMED NOTATION, OMITTED
--            PARAMETERS MUST HAVE DEFAULT VALUES.

-- JWC 7/17/85

PROCEDURE B95082E IS

     TASK T IS
          ENTRY E (X1: INTEGER; X2: INTEGER := 2; X3: INTEGER := 3);
     END T;

     TASK BODY T IS
     BEGIN
          NULL;
     END T;

BEGIN

     T.E (1,,);            -- ERROR: (A).
     NULL;

END B95082E;
