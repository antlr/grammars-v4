-- B95080C.ADA

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
-- CHECK THAT THE BASE TYPES OF CORRESPONDING FORMAL AND ACTUAL
-- PARAMETERS MUST BE THE SAME.

-- JWC 7/16/85
-- JRK 8/21/85

PROCEDURE B95080C IS

     TYPE NEWINT IS NEW INTEGER;
     SUBTYPE SNEWINT IS NEWINT RANGE 1..10;

     TYPE MAT1 IS ARRAY (NEWINT RANGE <>, NEWINT RANGE <>) OF
                         INTEGER;

     TYPE MAT2 IS ARRAY (NEWINT RANGE <>, NEWINT RANGE <>) OF
                         INTEGER;

     TYPE REC1 (I : SNEWINT := 1) IS
          RECORD
               M1 : MAT1 (1..I, 1..I);
          END RECORD;

     TYPE REC2 (I : SNEWINT := 1) IS
          RECORD
               M2 : MAT2 (1..I, 1..I);
        END RECORD;

     SUBTYPE INT1_10 IS INTEGER RANGE 1..10;
     SUBTYPE NEWINT1_10 IS NEWINT RANGE 1..10;
     SUBTYPE INT IS INTEGER;
     SUBTYPE MAT1_2 IS MAT1 (1..2, 1..2);
     SUBTYPE MAT2_2 IS MAT2 (1..2, 1..2);
     SUBTYPE REC1_2 IS REC1 (2);
     SUBTYPE REC2_2 IS REC2 (2);

     I    : INTEGER      := 15;
     N    : NEWINT       := 15;
     I1   : INT1_10      :=  5;
     N1   : NEWINT1_10   :=  6;
     M1   : MAT1_2       := ((1,3), (5,7));
     M2   : MAT2_2       := ((2,4), (6,8));
     R1   : REC1_2       := (2, ((1,2), (3,4)));
     R2   : REC2_2       := (2, ((1,2), (3,4)));
     J    : INT          := 15;


     TASK T IS

          ENTRY E_INTEGER (X : IN INTEGER);

          ENTRY E_NEWINT (X : OUT NEWINT);

          ENTRY E_MAT1 (X : IN OUT MAT1);

          ENTRY E_REC1 (X : IN REC1);

          ENTRY E_INT1_10 (X : OUT INT1_10);

          ENTRY E_INT (X : IN OUT INT);

          ENTRY E_MAT1_2 (X : IN MAT1_2);

          ENTRY E_REC1_2 (X : OUT REC1_2);

          ENTRY EF_INTEGER (1..3) (X : IN INTEGER);

          ENTRY EF_NEWINT (1..3) (X : OUT NEWINT);

          ENTRY EF_MAT1 (1..3) (X : IN OUT MAT1);

          ENTRY EF_REC1 (1..3) (X : IN REC1);

          ENTRY EF_INT1_10 (1..3) (X : OUT INT1_10);

          ENTRY EF_INT (1..3) (X : IN OUT INT);

          ENTRY EF_MAT1_2 (1..3) (X : IN MAT1_2);

          ENTRY EF_REC1_2 (1..3) (X : OUT REC1_2);

     END T;

     TASK BODY T IS
     BEGIN
          NULL;
     END T;

BEGIN

     T.E_INTEGER (N);      -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.E_INTEGER (1.0);    -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.E_INTEGER (N1);     -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.E_NEWINT (I);       -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.E_NEWINT (I1);      -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.E_NEWINT (J);       -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.E_MAT1 (M2);        -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.E_REC1 (R2);        -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.E_INT1_10 (N);      -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.E_INT1_10 (N1);     -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.E_INT (N);          -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.E_INT (N1);         -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.E_MAT1_2 (M2);      -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.E_MAT1_2 (R2.M2);   -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.E_INTEGER ('A');    -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.E_INTEGER (TRUE);   -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.E_INT (M1);         -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;

     T.EF_INTEGER (2) (N); -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.EF_INTEGER (2) (1.0); -- ERROR: DIFFERENT ACTUAL/FORMAL BASE
                             --        TYPES.
     NULL;
     T.EF_INTEGER (2) (N1); -- ERROR: DIFFERENT ACTUAL/FORMAL BASE
                            --        TYPES.
     NULL;
     T.EF_NEWINT (2) (I);  -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.EF_NEWINT (2) (I1); -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.EF_NEWINT (2) (J);  -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.EF_MAT1 (2) (M2);   -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.EF_REC1 (2) (R2);   -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.EF_INT1_10 (2) (N); -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.EF_INT1_10 (2) (N1); -- ERROR: DIFFERENT ACTUAL/FORMAL BASE
                            --        TYPES.
     NULL;
     T.EF_INT (2) (N);     -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.EF_INT (2) (N1);    -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.EF_MAT1_2 (2) (M2); -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     T.EF_MAT1_2 (2) (R2.M2); -- ERROR: DIFFERENT ACTUAL/FORMAL BASE
                              --        TYPES.
     NULL;
     T.EF_INTEGER (2) ('A'); -- ERROR: DIFFERENT ACTUAL/FORMAL BASE
                             --        TYPES.
     NULL;
     T.EF_INTEGER (2) (TRUE); -- ERROR: DIFFERENT ACTUAL/FORMAL BASE
                              --        TYPES.
     NULL;
     T.EF_INT (2) (M1);    -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;

END B95080C;
