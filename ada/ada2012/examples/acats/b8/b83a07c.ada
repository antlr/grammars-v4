-- B83A07C.ADA

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
--     CHECK THAT A STATEMENT LABEL IN A PACKAGE BODY
--     CANNOT BE THE SAME AS A BLOCK OR LOOP IDENTIFIER,
--     VARIABLE, CONSTANT, NAMED NUMBER, SUBPROGRAM, TYPE,
--     PACKAGE, EXCEPTION, OR GENERIC UNIT DECLARED IN
--     THE PACKAGE.

-- HISTORY:
--     SDA 09/08/88  CREATED ORIGINAL TEST.

PROCEDURE B83A07C IS

     PACKAGE TST IS
          PROCEDURE PRO;

          PACKAGE PKG IS
               PROCEDURE PROC_2;
          END PKG;
          GENERIC
               TYPE ELEMENT IS PRIVATE;
          PACKAGE PKG_GEN IS
               PROCEDURE PROC (ITEM : IN OUT ELEMENT);
          END PKG_GEN;
     END TST;

     PACKAGE BODY TST IS

          A : INTEGER := 0;
          B : CONSTANT := 5;
          C : CONSTANT INTEGER := 5;
          TYPE D IS (ON,OFF);
          EX : EXCEPTION;

          PROCEDURE PRO IS
          BEGIN
               NULL;
          END PRO;


          PACKAGE BODY PKG_GEN IS

               PROCEDURE PROC (ITEM : IN OUT ELEMENT) IS

               BEGIN
                    NULL;
               END PROC;

          END PKG_GEN;


          PACKAGE BODY PKG IS

               PROCEDURE PROC_2 IS

               BEGIN
                    NULL;
               END PROC_2;

          END PKG;


     BEGIN
          <<EX>>       -- ERROR: EXCEPTION CAN NOT BE USED AS A LABEL.
               NULL;
     L:   LOOP
               EXIT L;
          END LOOP L;
          <<L>>        -- ERROR: LOOP IDENTIFIER CAN NOT BE USED AS A
                       -- LABEL.
               NULL;
          <<A>>        -- ERROR: VARIABLE CAN NOT BE USED AS A LABEL.
               NULL;
          <<B>>        -- ERROR: NAMED NUMBER CAN NOT BE USED AS A
                       -- LABEL.
               NULL;
          <<C>>        -- ERROR: CONSTANT CAN NOT BE USED AS A LABEL.
               NULL;
          <<D>>        -- ERROR: TYPE CAN NOT BE USED AS A LABEL.
               NULL;
          <<PKG_GEN>>  -- ERROR: GENERIC PACKAGE CAN NOT BE USED AS
                       -- A LABEL.
               NULL;
          <<PKG>>      -- ERROR: PACKAGE CAN NOT BE USED AS A LABEL.
               NULL;
          <<PRO>>      -- ERROR: SUBPROGRAM CAN NOT BE USED AS A LABEL.
               NULL;
     END TST;

BEGIN
     NULL;
END B83A07C;
