-- BC3304A.ADA

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
-- PARAMETER MUST BE A FIXED POINT TYPE IF THE CORRESPONDING GENERIC 
-- FORMAL TYPE PARAMETER IS DEFINED BY A GENERIC TYPE DEFINITION
-- OF FORM "DELTA <>".

-- ASL 8/24/81

PROCEDURE BC3304A IS

     TYPE FIXED IS DELTA 1.0 RANGE 1.0 .. 9.0;
     TYPE NEW_FIXED IS NEW FIXED;
     SUBTYPE FIXED_ST IS FIXED RANGE 1.0 .. 2.0;
     TYPE NEW_FIXED_ST IS NEW FIXED_ST;
 
     TYPE COLOR IS (RED,BLUE,YELLOW);

     TYPE ARR IS ARRAY(INTEGER RANGE 1..1) OF FIXED;
     SUBTYPE STR IS STRING(1..1);

     TYPE REC IS
          RECORD
               COMP : FIXED;
          END RECORD;

     TYPE ACC IS ACCESS FIXED;

     PACKAGE P IS
          TYPE PRIV IS PRIVATE;
          TYPE LIM IS LIMITED PRIVATE;
     PRIVATE
          TYPE PRIV IS NEW FIXED;
          TYPE LIM IS NEW FIXED;
     END;

     USE P;

     GENERIC
          TYPE GFT IS DELTA <>;
     PACKAGE GP IS
     END GP;

     PACKAGE OK1 IS NEW GP(FIXED);            -- OK.
     PACKAGE OK2 IS NEW GP(FIXED_ST);         -- OK.
     PACKAGE OK3 IS NEW GP(NEW_FIXED);        -- OK.
     PACKAGE OK4 IS NEW GP(NEW_FIXED_ST);     -- OK.

     PACKAGE Z0 IS NEW GP(FLOAT);             -- ERROR: FLOAT.
     PACKAGE Z1 IS NEW GP(INTEGER);           -- ERROR: INTEGER.
     PACKAGE Z2 IS NEW GP(BOOLEAN);           -- ERROR: BOOLEAN.
     PACKAGE Z3 IS NEW GP(CHARACTER);         -- ERROR: CHARACTER.
     PACKAGE Z4 IS NEW GP(COLOR);             -- ERROR: COLOR.
     PACKAGE Z5 IS NEW GP(ARR);               -- ERROR: ARR.
     PACKAGE Z6 IS NEW GP(STR);               -- ERROR: STR.
     PACKAGE Z7 IS NEW GP(REC);               -- ERROR: REC.
     PACKAGE Z8 IS NEW GP(ACC);               -- ERROR: ACC.
     PACKAGE Z9 IS NEW GP(PRIV);              -- ERROR: PRIV.           
     PACKAGE Z10 IS NEW GP(LIM);              -- ERROR: LIM.
BEGIN
     NULL;
END BC3304A;
