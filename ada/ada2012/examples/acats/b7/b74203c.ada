-- B74203C.ADA

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
--     TYPES WITH LIMITED COMPONENTS.  THIS TEST CHECKS AN ARRAY WITH
--     LIMITED PRIVATE COMPONENTS AND A BOOLEAN ARRAY.

-- HISTORY:
--     BCB 04/10/90  CREATED ORIGINAL TEST FROM SPLIT OF B74203B.ADA.

PROCEDURE B74203C IS

     PACKAGE PP IS
          TYPE INT IS LIMITED PRIVATE;
     PRIVATE
          TYPE INT IS RANGE 1 .. 100;
     END PP;

     USE PP;

     PACKAGE P IS
          TYPE ARR1 IS LIMITED PRIVATE;
          TYPE ARR2 IS PRIVATE;

          CONS2 : CONSTANT ARR2;

          PROCEDURE CHECK (V : ARR2);
     PRIVATE
          TYPE ARR1 IS ARRAY(1..5) OF INT;

          TYPE ARR2 IS ARRAY(1..5) OF BOOLEAN;

          CONS2 : CONSTANT ARR2 := (TRUE,TRUE,TRUE,TRUE,TRUE);
     END P;

     USE P;

     VAL : INTEGER := 0;
     INT_VAR : INT;

     Y1, Y2, Y3 : ARR2 := CONS2;

     O1 : ARR1;

     PACKAGE BODY P IS
          PROCEDURE CHECK (V : ARR2) IS
          BEGIN
               NULL;
          END CHECK;
     END P;

BEGIN

     CHECK ((TRUE,FALSE,TRUE,FALSE,TRUE));  -- ERROR: AGGREGATES.

     IF O1(2) THEN         -- ERROR: INDEXED COMPONENTS.
          NULL;
     END IF;

     IF O1(2..4) THEN      -- ERROR: SLICING.
          NULL;
     END IF;

     IF INT_VAR IN O1'RANGE THEN -- ERROR: 'RANGE ATTRIBUTE NOT DEFINED.
          NULL;
     END IF;

     VAL := O1'LENGTH;     -- ERROR: 'LENGTH ATTRIBUTE NOT DEFINED.

     Y3 := Y1 & Y2;        -- ERROR: CATENATION.

     Y3 := NOT Y1;         -- ERROR: NOT OPERATOR NOT DEFINED.

     Y3 := Y1 AND Y2;      -- ERROR: AND OPERATOR NOT DEFINED.

     Y3 := Y1 OR Y2;       -- ERROR: OR OPERATOR NOT DEFINED.

     Y3 := Y1 XOR Y2;      -- ERROR: XOR OPERATOR NOT DEFINED.
END B74203C;
