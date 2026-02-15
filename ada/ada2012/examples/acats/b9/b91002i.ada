-- B91002I.ADA

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
-- CHECK THAT DECLARATIONS OTHER THAN ENTRY DECLARATIONS ARE NOT ALLOWED
-- IN TASK SPECIFICATIONS.

-- (I) PACKAGE DECLARATION.

-- JRK 9/16/81
-- JBG 4/19/83

PROCEDURE B91002I IS

     TASK TYPE T2I IS

          ENTRY E;

          PACKAGE PKG IS             -- ERROR: PROHIBITED IN TASK SPEC.
               I : INTEGER;
          END PKG;

     END T2I;

     TASK BODY T2I IS
     BEGIN
          NULL;
     END;

BEGIN
     NULL;
END B91002I;
