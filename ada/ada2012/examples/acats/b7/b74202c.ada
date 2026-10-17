-- B74202C.ADA

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
--     WHEN THE DESIGNATED TYPE OF AN ACCESS TYPE IS NON-LIMITED
--     PRIVATE, OPERATIONS FOR THE ACCESS TYPE WHICH DEPEND ON
--     CHARACTERISTICS OF THE FULL DECLARATION ARE NOT ACCESSIBLE
--     FROM OUTSIDE THE PACKAGE.

-- HISTORY:
--     BCB 07/28/88  CREATED ORIGINAL TEST.

PROCEDURE B74202C IS

     PACKAGE P IS
          TYPE ARR IS PRIVATE;
          TYPE REC (D : INTEGER) IS PRIVATE;
          TYPE ACC_ARR IS ACCESS ARR;
          TYPE ACC_REC IS ACCESS REC;
          C1 : CONSTANT ARR;
          D1 : CONSTANT REC;
     PRIVATE
          TYPE ARR IS ARRAY(1..3) OF INTEGER;

          TYPE REC (D : INTEGER) IS RECORD
               COMP1 : INTEGER;
               COMP2 : BOOLEAN;
          END RECORD;

          C1 : CONSTANT ARR := (1,2,3);
          D1 : CONSTANT REC := (1,2,FALSE);
     END P;

     USE P;

     TYPE Z IS ARRAY(1..2) OF INTEGER;

     AR, CR : ACC_ARR := NEW ARR'(C1);

     BR : ACC_REC := NEW REC'(D1);

     V : INTEGER;

     W : Z;

BEGIN

     V := AR(1);                          -- ERROR: INDEXING.

     W := AR(1..2);                       -- ERROR: SLICING.

     IF AR'FIRST /= 1 THEN                -- ERROR: 'FIRST ATTRIBUTE.
          NULL;
     END IF;

     IF AR'LAST /= 3 THEN                 -- ERROR: 'LAST ATTRIBUTE.
          NULL;
     END IF;

     IF AR'LENGTH /= 3 THEN               -- ERROR: 'LENGTH ATTRIBUTE.
          NULL;
     END IF;

     IF CR IN AR'RANGE THEN               -- ERROR: 'RANGE ATTRIBUTE.
          NULL;
     END IF;

     V := BR.COMP1;                       -- ERROR: COMPONENT SELECTION.

END B74202C;
