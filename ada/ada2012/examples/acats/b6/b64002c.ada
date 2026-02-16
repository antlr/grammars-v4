-- B64002C.ADA

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
-- CHECK THAT THE BASE TYPES OF A FORMAL AND ACTUAL PARAMETER CANNOT
--   BE DIFFERENT.

-- DAS 2/9/81
-- SPS 12/10/82
-- CPP 6/28/84
-- JRK 11/28/84

PROCEDURE B64002C IS

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
               M2 : MAT2 (1..I, 1..I)  ;
        END RECORD;

     SUBTYPE INT1_10 IS INTEGER  RANGE 1..10;
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


     PROCEDURE P_INTEGER (X : IN INTEGER) IS
     BEGIN
          NULL;
     END P_INTEGER;

     PROCEDURE P_NEWINT (X : OUT NEWINT) IS
     BEGIN
          NULL;
     END P_NEWINT;

     PROCEDURE P_MAT1 (X : IN OUT MAT1) IS
     BEGIN
          NULL;
     END P_MAT1;

     PROCEDURE P_REC1 (X : IN REC1) IS
     BEGIN
          NULL;
     END P_REC1;

     PROCEDURE P_INT1_10 (X : OUT INT1_10) IS
     BEGIN
          NULL;
     END P_INT1_10;

     PROCEDURE P_INT (X : IN OUT INT) IS
     BEGIN
          NULL;
     END P_INT;

     PROCEDURE P_MAT1_2 (X : IN MAT1_2) IS
     BEGIN
          NULL;
     END P_MAT1_2;

     PROCEDURE P_REC1_2 (X : OUT REC1_2) IS
     BEGIN
          NULL;
     END P_REC1_2;

BEGIN

     P_INTEGER (N);      -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     P_INTEGER (1.0);    -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     P_INTEGER (N1);     -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     P_NEWINT (I);       -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     P_NEWINT (I1);      -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     P_NEWINT (J);       -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     P_MAT1 (M2);        -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     P_REC1 (R2);        -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     P_INT1_10 (N);      -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     P_INT1_10 (N1);     -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     P_INT (N);          -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     P_INT (N1);         -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     P_MAT1_2 (M2);      -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     P_MAT1_2 (R2.M2);   -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     P_INTEGER ('A');    -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     P_INTEGER (TRUE);   -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;
     P_INT (M1);         -- ERROR: DIFFERENT ACTUAL/FORMAL BASE TYPES.
     NULL;

END B64002C;
