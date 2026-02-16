-- B74203D.ADA

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
--     CHECK THAT NO BASIC OPERATIONS THAT DEPEND ON THE FULL
--     DECLARATION OF THE TYPE ARE AVAILABLE FOR LIMITED AND NON-LIMITED
--     PRIVATE TYPES.  INCLUDE TYPES WITH DISCRIMINANTS AND PRIVATE
--     TYPES WITH LIMITED COMPONENTS.  THIS TEST CHECKS A RECORD TYPE,
--     AN ACCESS TYPE, A TASK TYPE, AND AN ARRAY TYPE DERIVED FROM A
--     PRIVATE TYPE.

-- HISTORY:
--     BCB 04/10/90  CREATED ORIGINAL TEST FROM SPLIT OF B74203B.ADA.

PROCEDURE B74203D IS

     PACKAGE PP IS
          TYPE PARENT IS PRIVATE;
     PRIVATE
          TYPE PARENT IS ARRAY(1..3) OF INTEGER;
     END PP;

     USE PP;

     PACKAGE P IS
          TYPE REC (D : INTEGER) IS PRIVATE;
          TYPE ACC IS PRIVATE;
          TYPE TSK IS LIMITED PRIVATE;
          TYPE CHILD IS NEW PARENT;

          TASK TYPE T IS
               ENTRY ONE;
          END T;

          CONS3 : CONSTANT REC;

          CONS4 : CONSTANT ACC;
     PRIVATE
          TYPE REC (D : INTEGER) IS RECORD
               COMP1 : INTEGER;
               COMP2 : BOOLEAN;
          END RECORD;

          TYPE ACC IS ACCESS INTEGER;

          TYPE TSK IS NEW T;

          CONS3 : CONSTANT REC := (0,1,FALSE);

          CONS4 : CONSTANT ACC := NEW INTEGER'(0);
     END P;

     USE P;

     BOOL : BOOLEAN := FALSE;
     VAL : INTEGER := 0;

     Z1 : REC(0) := CONS3;

     W1, W2 : ACC := CONS4;

     V1 : TSK;

     PACKAGE BODY PP IS
          VAL : INTEGER;
          PRIV_VAR, PRIV_VAR2, RES : CHILD;
     BEGIN

          IF PRIV_VAR(1) THEN               -- ERROR: INDEXING.
               NULL;
          END IF;

          IF PRIV_VAR(1..2) THEN            -- ERROR: SLICING.
               NULL;
          END IF;

          IF VAL IN PRIV_VAR'RANGE THEN     -- ERROR: 'RANGE ATTRIBUTE
               NULL;                        --        NOT DEFINED.
          END IF;

          VAL := PRIV_VAR'LENGTH;           -- ERROR: 'LENGTH ATTRIBUTE
                                            --        NOT DEFINED.

          RES := PRIV_VAR & PRIV_VAR2;      -- ERROR: CATENATION.

     END PP;

     PACKAGE BODY P IS
          TASK BODY T IS
          BEGIN
               ACCEPT ONE;
          END T;
     END P;

BEGIN

     VAL := Z1.COMP1;      -- ERROR: SELECTION OF RECORD COMPONENTS.

     W1 := NEW INTEGER'(0); -- ERROR: ALLOCATORS NOT DEFINED.

     W1 := NULL;            -- ERROR: NULL LITERAL NOT DEFINED.

     VAL := W2.ALL;         -- ERROR: SELECTED COMPONENT NOT DEFINED.

     BOOL := V1'CALLABLE;   -- ERROR: 'CALLABLE ATTRIBUTE NOT DEFINED.

     BOOL := V1'TERMINATED; -- ERROR: 'TERMINATED ATTRIBUTE NOT DEFINED.

     V1.ONE;                -- ERROR: SELECTION OF AN ENTRY.
END B74203D;
