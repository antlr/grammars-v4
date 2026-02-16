-- BC3005B.ADA

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
--     CHECK THAT, IN A GENERIC INSTANTIATION, THE ACTUAL PARAMETER 
--     CORRESPONDING TO A FORMAL TYPE CANNOT BE A SUBTYPE INDICATION 
--     WITH AN EXPLICIT RANGE CONSTRAINT, ACCURACY CONSTRAINT, INDEX 
--     CONSTRAINT, OR DISCRIMINANT CONSTRAINT.

-- HISTORY:
--     PWB  03/06/86
--     THS  09/21/90  MODIFIED HEADER FORMAT AND SPLIT TEST TO
--                    BC3005C.ADA.

PROCEDURE BC3005B IS

     TYPE FLOATING IS DIGITS 5 RANGE -10.0 .. 10.0;
     TYPE FIXED IS DELTA 0.25 RANGE -10.0 .. 10.0;
     TYPE ENUM IS (ONE, TWO, THREE);

     GENERIC
          TYPE FORMAL IS (<>);
     PACKAGE DISCRETE_PACK IS
     END DISCRETE_PACK;

     GENERIC
          TYPE FORMAL IS DELTA <>;
     PACKAGE FIXED_PACK IS
     END FIXED_PACK;

     GENERIC
          TYPE FORMAL IS RANGE <>;
     PROCEDURE INT_PROC;

     GENERIC
          TYPE FORMAL IS PRIVATE;
     PROCEDURE PRIV_PROC;

     GENERIC
          TYPE FORMAL IS DIGITS <>;
     FUNCTION FLOAT_FUNC RETURN BOOLEAN;

     GENERIC
          TYPE FORMAL IS LIMITED PRIVATE;
     FUNCTION LIM_FUNC RETURN BOOLEAN;

     PROCEDURE INT_PROC IS
     BEGIN
          NULL;
     END INT_PROC;

     PROCEDURE PRIV_PROC IS
     BEGIN
          NULL;
     END PRIV_PROC;

     FUNCTION FLOAT_FUNC RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END FLOAT_FUNC;

     FUNCTION LIM_FUNC RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END LIM_FUNC;

     PACKAGE DISCRETE_INST IS 
             NEW DISCRETE_PACK (ENUM RANGE ONE..TWO);   -- ERROR:
                                                        -- CONSTRAINT.

     PACKAGE FIXED_INST_1 IS
             NEW FIXED_PACK (FIXED RANGE -1.0..1.0);    -- ERROR:
                                                        -- CONSTRAINT.

     PACKAGE FIXED_INST_2 IS
             NEW FIXED_PACK (FIXED DELTA 0.5);          -- ERROR:
                                                        -- CONSTRAINT.

     PROCEDURE INT_INST IS
             NEW INT_PROC (INTEGER RANGE 0..100);       -- ERROR:
                                                        -- CONSTRAINT.

     PROCEDURE PRIV_INST_2 IS
             NEW PRIV_PROC (FIXED DELTA 0.5);           -- ERROR:
                                                        -- CONSTRAINT.

     FUNCTION FLOAT_INST IS
             NEW FLOAT_FUNC (FLOATING RANGE -1.0..1.0); -- ERROR:
                                                        -- CONSTRAINT.

     FUNCTION LIM_INST_2 IS
             NEW LIM_FUNC (FLOATING DIGITS 4);          -- ERROR:
                                                        -- CONSTRAINT.

BEGIN
     NULL;
END BC3005B;
