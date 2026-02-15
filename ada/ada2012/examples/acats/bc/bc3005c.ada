-- BC3005C.ADA

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
--     THS  09/21/90  CREATED TEST FROM SPLIT OF BC3005B.ADA.

PROCEDURE BC3005C IS

     TYPE ENUM IS (ONE, TWO, THREE);
     TYPE LIST IS ARRAY (INTEGER RANGE <>) OF ENUM;
     TYPE REC (DISC : INTEGER) IS RECORD
          NULL;
     END RECORD;

     GENERIC
          TYPE FORMAL IS ARRAY (INTEGER RANGE <>) OF ENUM;
     PACKAGE ARRAY_PACK IS
     END ARRAY_PACK;

     GENERIC
          TYPE FORMAL IS PRIVATE;
     PROCEDURE PRIV_PROC;

     GENERIC
          TYPE FORMAL IS LIMITED PRIVATE;
     FUNCTION LIM_FUNC RETURN BOOLEAN;

     PROCEDURE PRIV_PROC IS
     BEGIN
          NULL;
     END PRIV_PROC;

     FUNCTION LIM_FUNC RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END LIM_FUNC;

     PACKAGE ARRAY_INST IS 
             NEW ARRAY_PACK ( LIST(1..6) );             -- ERROR:
                                                        -- CONSTRAINT.

     PROCEDURE PRIV_INST_1 IS
             NEW PRIV_PROC ( REC(DISC=>0) );            -- ERROR:
                                                        -- CONSTRAINT.

     FUNCTION LIM_INST_1 IS
             NEW LIM_FUNC ( REC(1) );                   -- ERROR:
                                                        -- CONSTRAINT.

BEGIN
     NULL;
END BC3005C;
