-- BD2D03A.ADA

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
--     CHECK THAT A 'SMALL SPECIFICATION CANNOT BE GIVEN IN A PACKAGE
--     SPECIFICATION FOR A TYPE DECLARED IN AN INNER PACKAGE
--     SPECIFICATION; IN A PACKAGE OR TASK SPECIFICATION FOR A TYPE
--     DECLARED IN AN ENCLOSING PACKAGE SPECIFICATION; OR IN A PACKAGE
--     BODY FOR A TYPE DECLARED IN THE CORRESPONDING SPECIFICATION.

-- HISTORY:
--     BCB 04/05/88  CREATED ORIGINAL TEST.
--     BCB 03/29/90  SPLIT ORIGINAL TEST INTO BD2D03A.ADA, BD2D03B.ADA,
--                   AND BD2D03C.ADA.

PROCEDURE BD2D03A IS

     CHECK_SMALL : CONSTANT := 0.25;

     PACKAGE P IS
          TYPE PFIX IS DELTA 2.0**(-1) RANGE -1.0 .. 1.0;
          TYPE PTFIX IS DELTA 2.0**(-1) RANGE -1.0 .. 1.0;
          TYPE PSFIX IS DELTA 2.0**(-1) RANGE -1.0 .. 1.0;

          PACKAGE Q IS
               TYPE QFIX IS DELTA 2.0**(-1) RANGE -1.0 .. 1.0;

               FOR PFIX'SMALL USE CHECK_SMALL;  -- ERROR: TYPE DECLARED
                                                -- IN ENCLOSING PACKAGE.
          END Q;
          USE Q;

          TASK T IS
               FOR PTFIX'SMALL USE CHECK_SMALL; -- ERROR: TYPE DECLARED
                                                -- IN ENCLOSING PACKAGE.
          END T;

          FOR QFIX'SMALL USE CHECK_SMALL;       -- ERROR: TYPE DECLARED
                                                -- IN INNER PACKAGE.
     END P;

     PACKAGE BODY P IS
          FOR PSFIX'SMALL USE CHECK_SMALL;    -- ERROR: TYPE DECLARED IN
                                              -- PACKAGE SPECIFICATION.

          TASK BODY T IS
          BEGIN
               NULL;
          END T;

     BEGIN
          NULL;
     END P;

BEGIN
     NULL;
END BD2D03A;
