-- BC3401A.ADA

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
-- CHECK THAT IN A GENERIC INSTANTIATION,  A GENERIC ACTUAL TYPE
-- PARAMETER MUST BE AN ARRAY TYPE IF THE CORRESPONDING GENERIC
-- FORMAL TYPE PARAMETER IS DEFINED BY A GENERIC ARRAY TYPE
-- DEFINITION.

-- ASL 9/3/81

PROCEDURE BC3401A IS

     TYPE ARR IS ARRAY(INTEGER) OF INTEGER;
     TYPE NEW_ARR IS NEW ARR;

     TYPE COLOR IS (RED,BLUE,YELLOW);

     TYPE REC IS
          RECORD
               COMP : ARR;
          END RECORD;

     TYPE ACC IS ACCESS ARR;

     PACKAGE P IS
          TYPE PRIV IS PRIVATE;
          TYPE LIM IS LIMITED PRIVATE;
     PRIVATE
          TYPE PRIV IS NEW ARR;
          TYPE LIM IS NEW ARR;
     END;

     USE P;

     GENERIC
          TYPE GFT IS ARRAY(INTEGER) OF INTEGER;
     PACKAGE GP IS
     END GP;

     PACKAGE OK1 IS NEW GP(ARR);              -- OK.
     PACKAGE OK2 IS NEW GP(NEW_ARR);          -- OK.

     PACKAGE Z1 IS NEW GP(FLOAT);             -- ERROR: FLOAT.
     PACKAGE Z2 IS NEW GP(INTEGER);           -- ERROR: INTEGER.
     PACKAGE Z3 IS NEW GP(BOOLEAN);           -- ERROR: BOOLEAN.
     PACKAGE Z4 IS NEW GP(CHARACTER);         -- ERROR: CHARACTER.
     PACKAGE Z5 IS NEW GP(COLOR);             -- ERROR: COLOR.
     PACKAGE Z6 IS NEW GP(STRING);            -- ERROR: STR.
     PACKAGE Z7 IS NEW GP(REC);               -- ERROR: REC.
     PACKAGE Z8 IS NEW GP(ACC);               -- ERROR: ACC.
     PACKAGE Z9 IS NEW GP(PRIV);              -- ERROR: PRIV.   
     PACKAGE Z10 IS NEW GP(LIM);              -- ERROR: LIM.
BEGIN
     NULL;
END BC3401A;
