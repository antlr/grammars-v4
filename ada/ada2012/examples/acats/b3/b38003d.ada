-- B38003D.ADA

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
--      CHECK THAT IF AN INDEX OR DISCRIMINANT CONSTRAINT IS PROVIDED IN
--      AN ACCESS TYPE DEFINITION (OR IF THE SUBTYPE INDICATION IS
--      ALREADY CONSTRAINED), THE ACCESS TYPE NAME CANNOT SUBSEQUENTLY
--      BE USED WITH AN INDEX OR DISCRIMINANT CONSTRAINT, EVEN IF THE
--      SAME CONSTRAINT VALUES ARE USED.
--
--      CHECK GENERIC FORMAL ACCESS TYPES WHEN SUBTYPE INDICATION IS
--      ALREADY CONSTRAINED.
--
-- CASES:
--     F) PARAMETER DECLARATION
--     G) RETURN TYPE IN FUNCTION DECLARATION

-- HISTORY:
--      MCH 05/17/90  SPLIT FROM B38003B.ADA.
--      RLB 06/24/16  REPAIRED INCORRECT ERROR TAGS.
PROCEDURE B38003D IS

     DISC_VAL : INTEGER := 0;
     TYPE REC1(DISC1 : INTEGER) IS
          RECORD
               NULL;
          END RECORD;
     SUBTYPE CON_REC IS REC1(DISC_VAL);

     GENERIC
          TYPE INDEX IS RANGE <>;

          TYPE LIST IS ARRAY (INDEX) OF BOOLEAN;
          TYPE A_LIST IS ACCESS LIST;

          TYPE A_CON_REC IS ACCESS CON_REC;

     PACKAGE P IS

          PROCEDURE PROC1 (PARM1 : A_LIST (INDEX));         -- ERROR: F.
          PROCEDURE PROC2 (PARM2 : A_CON_REC (DISC_VAL));   -- ERROR: F.

          FUNCTION FUNC1 RETURN A_LIST (INDEX);          -- ERROR: G.
          FUNCTION FUNC2 RETURN A_CON_REC (DISC_VAL);    -- ERROR: G.

     END P;

     PACKAGE BODY P IS
          PROCEDURE PROC1 (PARM1 : A_LIST (INDEX)) IS     -- OPTIONAL ERROR: F.

          BEGIN
               NULL;
          END PROC1;

          PROCEDURE PROC2 (PARM2 : A_CON_REC (DISC_VAL)) IS-- OPTIONAL ERROR: F

          BEGIN
               NULL;
          END PROC2;

          FUNCTION FUNC1 RETURN A_LIST (INDEX) IS         -- OPTIONAL ERROR: G.

          BEGIN
               RETURN NULL;
          END FUNC1;

          FUNCTION FUNC2 RETURN A_CON_REC (DISC_VAL) IS   -- OPTIONAL ERROR: G.

          BEGIN
               RETURN NULL;
          END FUNC2;

     END P;

BEGIN
     NULL;
END B38003D;
