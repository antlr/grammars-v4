-- B36001A.ADA

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
--     CHECK THAT AN ARRAY TYPE CANNOT BE DECLARED WITH
--     A NON-DISCRETE INDEX TYPE.

-- HISTORY:
--     L.BROWN  7/1/86
--     DWC  07/24/87     CHANGED ERROR MESSAGE FROM 'BOOLEAN' TO 'FLT'.

PROCEDURE B36001A IS

     TYPE FIX IS DELTA 0.1 RANGE -1.0 .. 1.0;
     TYPE ARR_FIX IS
          ARRAY (FIX RANGE <>) OF INTEGER;            -- ERROR: FIX.

     TYPE ARR_FLOT IS
          ARRAY (FLOAT RANGE <>) OF INTEGER;          -- ERROR: FLOAT.

     TYPE FLT IS DIGITS 5 RANGE -1.0 .. 1.0;
     TYPE ARR_FLT IS
          ARRAY (FLT RANGE 0.0 .. 1.0) OF INTEGER;    -- ERROR: FLT.

     TYPE ARR_FL IS ARRAY (FLOAT) OF BOOLEAN;         -- ERROR: FLOAT.

     TYPE ARR_FL1 IS ARRAY (FLT) OF INTEGER;          -- ERROR: FLT.

     TYPE ARR_FIXD IS ARRAY (FIX) OF INTEGER;         -- ERROR: FIX.

     SUBTYPE STR IS STRING(1 .. 10);

     TYPE ARRY IS ARRAY (1 .. 1) OF INTEGER;

     TYPE REC IS
          RECORD
               INT : INTEGER;
          END RECORD;
     TYPE ARR_REC IS ARRAY(REC) OF INTEGER;           -- ERROR: REC.

     TYPE ARR_ARRY IS ARRAY(ARRY) OF INTEGER;         -- ERROR: ARRY.

     TYPE ARR_CHAR IS ARRAY(STR) OF CHARACTER;        -- ERROR: STR.

     TYPE AC_INT IS ACCESS BOOLEAN;
     TYPE AC_ARR IS ARRAY(AC_INT) OF INTEGER;         -- ERROR: AC_INT.

     TASK TYPE T_TYPE;

     ARR_FT : ARRAY(FLOAT RANGE -1.0 .. 1.0)          -- ERROR: FLOAT.
                    OF BOOLEAN;
     ARR_FX : ARRAY(FIX RANGE -1.0 .. 1.0)            -- ERROR: FIX.
                    OF INTEGER;
     ARR_FTL : ARRAY(FLT RANGE -1.0 .. 1.0)           -- ERROR: FLT.
                     OF BOOLEAN;

     ARR_RC : ARRAY(REC) OF BOOLEAN;                  -- ERROR: REC.

     ARR_AR : ARRAY(ARRY) OF INTEGER;                 -- ERROR: ARRY.

     ARR_TSK : ARRAY(T_TYPE) OF INTEGER;              -- ERROR: T_TYPE.

     TASK BODY T_TYPE IS
          BEGIN
               NULL;
          END;
BEGIN
     NULL;
END B36001A;
