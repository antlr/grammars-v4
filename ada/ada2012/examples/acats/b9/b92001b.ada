-- B92001B.ADA

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
-- CHECK THAT TASK OBJECTS AND OBJECTS HAVING A SUBCOMPONENT OF A TASK
-- TYPE CANNOT BE ASSIGNED OR COMPARED FOR EQUALITY OR INEQUALITY.

--   TBN  30 Jan 1986
--   RLB  23 Mar 2007  Limited Initializations can be legal.

PROCEDURE B92001B IS

     TASK TYPE T1;

     OBJ_T1 : T1;
     OBJ_T2 : T1 := OBJ_T1;                         -- ERROR: Init w/ OBJ_T1
     OBJ_T3 : T1;

     TYPE MY_REC1 IS
          RECORD
               ONE : T1;
          END RECORD;

     OBJ_REC1 : MY_REC1 := (ONE => OBJ_T1);         -- ERROR: Init w/ OBJ_T1
     OBJ_REC4, OBJ_REC5 : MY_REC1;

     TYPE MY_REC2 IS
          RECORD
               ONE : T1 := OBJ_T1;                  -- ERROR: Init w/ OBJ_T1
          END RECORD;

     TYPE MY_ARRAY1 IS ARRAY (1 .. 2) OF T1;

     OBJ_ARA1 : MY_ARRAY1 := (OBJ_T1, OBJ_T1);      -- ERROR: Init w/ OBJ_T1
     OBJ_ARA3, OBJ_ARA4 : MY_ARRAY1;

     TYPE MY_ARRAY2 IS ARRAY (1 .. 2) OF MY_REC1;

     OBJ_ARA2 : MY_ARRAY2 := ((ONE=> OBJ_T1),(ONE=> OBJ_T1));  -- ERROR:
                                                    -- Init w/ OBJ_T1
     OBJ_ARA5, OBJ_ARA6 : MY_ARRAY2;

     TYPE MY_REC3 IS
          RECORD
               NEXT : MY_ARRAY1;
          END RECORD;

     OBJ_REC3 : MY_REC3 := (NEXT => (OBJ_T1, OBJ_T1));  -- ERROR:
                                                    -- Init w/ OBJ_T1
     OBJ_REC6, OBJ_REC7 : MY_REC3;

     TASK BODY T1 IS
     BEGIN
          NULL;
     END T1;


BEGIN
     OBJ_T3 := OBJ_T1;                        -- ERROR: ASSIGNMENT.
     OBJ_ARA3 := OBJ_ARA4;                    -- ERROR: ASSIGNMENT.
     OBJ_REC4 := OBJ_REC5;                    -- ERROR: ASSIGNMENT.
     OBJ_ARA5 := OBJ_ARA6;                    -- ERROR: ASSIGNMENT.
     OBJ_REC6 := OBJ_REC7;                    -- ERROR: ASSIGNMENT.

     IF OBJ_T3 = OBJ_T1 THEN                  -- ERROR: EQUALITY.
          NULL;
     END IF;
     IF OBJ_T3 /= OBJ_T1 THEN                 -- ERROR: INEQUALITY.
          NULL;
     END IF;
     IF OBJ_ARA3 = OBJ_ARA4 THEN              -- ERROR: EQUALITY.
          NULL;
     END IF;
     IF OBJ_ARA3 /= OBJ_ARA4 THEN             -- ERROR: INEQUALITY.
          NULL;
     END IF;
     IF OBJ_REC4 = OBJ_REC5 THEN              -- ERROR: EQUALITY.
          NULL;
     END IF;
     IF OBJ_REC4 /= OBJ_REC5 THEN             -- ERROR: INEQUALITY.
          NULL;
     END IF;
     IF OBJ_ARA5 = OBJ_ARA6 THEN              -- ERROR: EQUALITY.
          NULL;
     END IF;
     IF OBJ_ARA5 /= OBJ_ARA6 THEN             -- ERROR: INEQUALITY.
          NULL;
     END IF;
     IF OBJ_REC6 = OBJ_REC7 THEN              -- ERROR: EQUALITY.
          NULL;
     END IF;
     IF OBJ_REC6 /= OBJ_REC7 THEN             -- ERROR: INEQUALITY.
          NULL;
     END IF;

END B92001B;
