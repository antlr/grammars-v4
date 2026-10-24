-- B74404A.ADA

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
-- CHECK THAT VARIABLES AND CONSTANTS OF A LIMITED PRIVATE TYPE CANNOT
-- BE COMPARED FOR EQUALITY (OR INEQUALITY) OUTSIDE THE PACKAGE DEFINING
-- THE TYPE, NOR CAN THEY BE ASSIGNED TO OR GIVEN INITIAL VALUES THAT ARE
-- NOT AGGREGATES OR FUNCTION CALLS.

-- RJW  1/07/86
-- RLB  3/20/07 - Corrected objective for Amendment 1 changes.

PROCEDURE B74404A IS

     PACKAGE PKG IS

          TYPE LIM_TYPE IS LIMITED PRIVATE;
          TYPE LIM_PTR_TYPE IS LIMITED PRIVATE;
          TYPE PTR_TYPE IS ACCESS LIM_TYPE;

          EC1, EC2 : CONSTANT LIM_TYPE;

     PRIVATE

          TYPE LIM_TYPE IS NEW INTEGER;
          TYPE LIM_PTR_TYPE IS ACCESS LIM_TYPE;

          EC1, EC2 : CONSTANT LIM_TYPE := 3;

     END PKG;

     USE PKG;

     E1, E2 : LIM_TYPE;
     LP1, LP2 : LIM_PTR_TYPE;
     P1, P2 : PTR_TYPE;
     E3 : LIM_TYPE := EC1;      -- ERROR: ILLEGAL INITIALIZATION.

BEGIN

     E1 := EC1;                 -- ERROR: ASSIGNMENT NOT ALLOWED.
     E1 := 2;                   -- ERROR: TYPE MISMATCH.
     LP1 := LP2;                -- ERROR: ASSIGNMENT NOT ALLOWED.
     P1.ALL := P2.ALL;          -- ERROR: ASSIGNMENT NOT ALLOWED.

     IF EC1 = EC2 THEN          -- ERROR: COMPARISON NOT ALLOWED.
          NULL;
     END IF;

     IF E1 /= E2 THEN           -- ERROR: COMPARISON NOT ALLOWED.
          NULL;
     END IF;

     IF E1 = 2 THEN             -- ERROR: TYPE MISMATCH.
          NULL;
     END IF;

     IF LP1 = LP2 THEN          -- ERROR: COMPARISON NOT ALLOWED.
          NULL;
     END IF;

     IF P1.ALL /= P2.ALL THEN   -- ERROR: COMPARISON NOT ALLOWED.
          NULL;
     END IF;

END B74404A;
