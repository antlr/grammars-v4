-- B38008B.ADA

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
-- CHECK THAT A CONSTRAINED ACCESS SUBTYPE CANNOT BE FURTHER
-- CONSTRAINED IN A SUBTYPE INDICATION, EVEN IF THE SAME CONSTRAINT
-- VALUES ARE USED.
 
-- ASL 6/24/81
-- SPS 12/7/82

PROCEDURE B38008B IS
 
     TYPE ARR IS ARRAY(INTEGER RANGE <>) OF INTEGER;
     TYPE ARR_NAME IS ACCESS ARR;
     SUBTYPE SUB_ARR IS ARR_NAME(1..5);
     TYPE SUB2_ARR IS ACCESS SUB_ARR;
 
     TYPE REC(DISC : INTEGER) IS
          RECORD
               NULL;
          END RECORD;
 
     TYPE REC_NAME IS ACCESS REC;
 
     SUBTYPE SUB_REC IS REC_NAME(5);
     TYPE SUB2_REC IS ACCESS SUB_REC;
 
     OBJ1 : SUB_ARR(1..5);                         -- ERROR: CONSTRAINT.
     OBJ2 : SUB_REC(5);                            -- ERROR: CONSTRAINT.
 
     TYPE ARR2 IS ARRAY(1..10) OF SUB_ARR(1..5);   -- ERROR: CONSTRAINT.
     TYPE ARR3 IS ARRAY(1..10) OF SUB_REC(5);      -- ERROR: CONSTRAINT.
 
     TYPE REC1 IS
          RECORD
               COMP1 : SUB_ARR(1..5);              -- ERROR: CONSTRAINT.
               COMP2 : SUB_REC(5);                 -- ERROR: CONSTRAINT.
          END RECORD;
 
     TYPE AA_NAME IS ACCESS SUB_ARR(1..5);         -- ERROR: CONSTRAINT.
  
     TYPE DER1 IS NEW SUB_ARR(1..5);               -- ERROR: CONSTRAINT.
     TYPE DER2 IS NEW SUB_REC(5);                  -- ERROR: CONSTRAINT.
 
     A1 : SUB2_ARR := NEW SUB_ARR(1..5);           -- ERROR: CONSTRAINT.
     A2 : SUB2_REC := NEW SUB_REC(DISC => 5);      -- ERROR: CONSTRAINT.
     A3 : SUB2_REC := NEW SUB_REC(5);              -- ERROR: CONSTRAINT.
 
     
     
 
BEGIN
     NULL;
END B38008B;
