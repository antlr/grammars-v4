-- BC3302A.ADA

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
-- CHECK THAT IN A GENERIC INSTANTIATION, A GENERIC ACTUAL TYPE
-- PARAMETER MUST BE AN INTEGER TYPE IF THE CORRESPONDING GENERIC 
-- FORMAL TYPE PARAMETER IS DEFINED BY A GENERIC TYPE DEFINITION
-- OF FORM "RANGE <>".

-- ASL 8/24/81

PROCEDURE BC3302A IS

     TYPE NEW_INT IS NEW INTEGER;
     SUBTYPE SUB_INT IS INTEGER RANGE 1..5;
     TYPE NEW_SUB_INT IS NEW SUB_INT;

     TYPE COLOR IS (RED,BLUE,YELLOW);

     TYPE ARR IS ARRAY(INTEGER RANGE 1..1) OF INTEGER;
     SUBTYPE STR IS STRING(1..1);

     TYPE REC(D : SUB_INT) IS
          RECORD
               COMP : NEW_SUB_INT;
          END RECORD;

     TYPE ACC IS ACCESS INTEGER;

     PACKAGE P IS
          TYPE PRIV IS PRIVATE;
          TYPE LIM IS LIMITED PRIVATE;
     PRIVATE
          TYPE PRIV IS NEW INTEGER;
          TYPE LIM IS NEW SUB_INT;
     END;

     USE P;

     GENERIC
          TYPE GFT IS RANGE <>;
     PACKAGE GP IS
     END GP;

     PACKAGE OK1 IS NEW GP(INTEGER);         -- OK.
     PACKAGE OK2 IS NEW GP(NEW_INT);         -- OK.
     PACKAGE OK3 IS NEW GP(SUB_INT);         -- OK.
     PACKAGE OK4 IS NEW GP(NEW_SUB_INT);     -- OK.

     PACKAGE Z1 IS NEW GP(FLOAT);            -- ERROR: FLOAT.
     PACKAGE Z2 IS NEW GP(ARR);              -- ERROR: ARR.
     PACKAGE Z3 IS NEW GP(STR);              -- ERROR: STR.
     PACKAGE Z4 IS NEW GP(REC);              -- ERROR: REC.
     PACKAGE Z5 IS NEW GP(ACC);              -- ERROR: ACC.
     PACKAGE Z6 IS NEW GP(PRIV);             -- ERROR: PRIV.           
     PACKAGE Z7 IS NEW GP(LIM);              -- ERROR: LIM.
     PACKAGE Z8 IS NEW GP(BOOLEAN);          -- ERROR: BOOLEAN.
     PACKAGE Z9 IS NEW GP(CHARACTER);        -- ERROR: CHARACTER.
     PACKAGE Z10 IS NEW GP(COLOR);           -- ERROR: COLOR.
BEGIN
     NULL;
END BC3302A;
