-- BC3607A.ADA

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
--     CHECK THAT IF A DEFAULT SUBPROGRAM IS SPECIFIED WITH
--     A BOX, THE MODES OF CORRESPONDING PARAMETERS MUST
--     MATCH.

-- HISTORY:
--     LDC 08/17/88  CREATED ORIGINAL TEST

PROCEDURE BC3607A IS

BEGIN
     DECLARE
          GENERIC
               TYPE PERSONAL_LIFE IS PRIVATE;
          PACKAGE LLZ_LDC IS
               GENERIC
                    WITH PROCEDURE LORI (LYNN : OUT PERSONAL_LIFE)
                         IS <>;

               PACKAGE ZWINK IS
               END ZWINK;

               GENERIC
                    WITH PROCEDURE LORI_2 (LYNN : BOOLEAN) IS <>;

               PACKAGE ZWINK_2 IS
               END ZWINK_2;

               GENERIC
                    WITH PROCEDURE LORI_3 (LYNN : INTEGER) IS <>;

               PACKAGE ZWINK_3 IS
               END ZWINK_3;

               PROCEDURE LORI   (LYNN  : PERSONAL_LIFE);
               PROCEDURE LORI_2 (LYNN  : OUT BOOLEAN);
               PROCEDURE LORI_3 (LYNN  : IN OUT INTEGER);

               PACKAGE CROCKETT IS NEW ZWINK;          -- ERROR: MODES
                                                       --   ARE NOT THE
                                                       --   SAME

               PACKAGE CROCKETT_2 IS NEW ZWINK_2;      -- ERROR: MODES
                                                       --   ARE NOT THE
                                                       --   SAME

               PACKAGE CROCKETT_3 IS NEW ZWINK_3;      -- ERROR: MODES
                                                       --   ARE NOT THE
                                                       --   SAME
          END LLZ_LDC;

          PACKAGE BODY LLZ_LDC IS
               PROCEDURE LORI (LYNN : PERSONAL_LIFE) IS
               BEGIN
                    NULL;
               END LORI;

               PROCEDURE LORI_2 (LYNN  : OUT BOOLEAN) IS
               BEGIN
                    LYNN := TRUE;
               END LORI_2;

               PROCEDURE LORI_3 (LYNN  : IN OUT INTEGER) IS
               BEGIN
                    NULL;
               END LORI_3;

          END LLZ_LDC;

     BEGIN
          NULL;
     END;
END BC3607A;
