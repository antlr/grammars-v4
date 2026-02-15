-- B38003B.ADA

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
--     A) OBJECT DECLARATION
--     B) ARRAY COMPONENT DECLARATION
--     C) RECORD COMPONENT DECLARATION
--     D) ACCESS TYPE DECLARATION
--     E) DERIVED TYPE DEFINITION

-- HISTORY:
--      AH  08/25/86  CREATED ORIGINAL TEST.
--      MCH 05/17/90  SPLIT AND CREATED B38003D.ADA.

PROCEDURE B38003B IS

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

          OBJ1 : A_LIST(INDEX);                         -- ERROR: A.
          OBJ2 : A_CON_REC(DISC_VAL);                   -- ERROR: A.

          TYPE ARR1 IS ARRAY(1..10) OF
                       A_LIST(INDEX);                   -- ERROR: B.
          TYPE ARR2 IS ARRAY(1..10) OF
                       A_CON_REC(DISC_VAL);             -- ERROR: B.

          TYPE REC2 IS
               RECORD
                    COMP1 : A_LIST(INDEX);              -- ERROR: C.
                    COMP2 : A_CON_REC(DISC_VAL);        -- ERROR: C.
               END RECORD;

          TYPE ACC1 IS ACCESS A_LIST(INDEX);            -- ERROR: D.
          TYPE ACC2 IS ACCESS A_CON_REC(DISC_VAL);      -- ERROR: D.

          TYPE DER1 IS NEW A_LIST(INDEX);               -- ERROR: E.
          TYPE DER2 IS NEW A_CON_REC(DISC_VAL);         -- ERROR: E.

     END P;

BEGIN
     NULL;
END B38003B;
