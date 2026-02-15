-- BD2C03A.TST

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
--     CHECK THAT A TASK STORAGE SIZE SPECIFICATION CANNOT BE GIVEN:
--     - IN A PACKAGE FOR TYPE DECLARED IN AN INNER PACKAGE
--       SPECIFICATION;
--     - IN A PACKAGE OR TASK SPECIFICATION FOR TYPE DECLARED IN AN
--       ENCLOSING PACKAGE OR DECLARATIVE PART;
--     - IN A PACKAGE BODY FOR A TYPE DECLARED IN THE CORRESPONDING
--       SPECIFICATION.

-- MACRO SUBSTITUTION:
--     $TASK_STORAGE_SIZE IS THE NUMBER OF STORAGE_UNITS REQUIRED FOR
--     THE ACTIVATION OF A TASK.

-- HISTORY:
--     JKC 04/06/88  CREATED ORIGINAL TEST.
--     BCB 04/14/89  CHANGED EXTENSION TO '.TST'.  ADDED A MACRO TO
--                   TASK STORAGE_SIZE CLAUSES.

PROCEDURE BD2C03A IS

     PACKAGE P IS
          TASK TYPE T2;
          TASK TYPE T6;

          PACKAGE INNER_P IS
               TASK TYPE T1;
          END INNER_P;

          USE INNER_P;

          FOR T1'STORAGE_SIZE USE $TASK_STORAGE_SIZE;       -- ERROR:

          PACKAGE INNER_Q IS
               FOR T2'STORAGE_SIZE USE $TASK_STORAGE_SIZE;  -- ERROR:
          END INNER_Q;
     END P;

     PACKAGE BODY P IS
          FOR T6'STORAGE_SIZE USE $TASK_STORAGE_SIZE;      -- ERROR:

          PACKAGE BODY INNER_P IS
               TASK BODY T1 IS
               BEGIN
                    NULL;
               END T1;
          BEGIN
               NULL;
          END INNER_P;

          TASK BODY T2 IS
          BEGIN
               NULL;
          END T2;

          TASK BODY T6 IS
          BEGIN
               NULL;
          END T6;
     END P;

     PACKAGE Q IS
          TASK TYPE T4;

          TASK T IS
               FOR T4'STORAGE_SIZE USE $TASK_STORAGE_SIZE; -- ERROR:
          END T;
     END Q;

     PACKAGE BODY Q IS
          TASK BODY T IS
          BEGIN
               NULL;
          END T;

          TASK BODY T4 IS
          BEGIN
               NULL;
          END T4;
     END Q;

BEGIN
     DECLARE
          TASK TYPE T3;

          PACKAGE S IS
               FOR T3'STORAGE_SIZE USE $TASK_STORAGE_SIZE; -- ERROR:
          END S;

          TASK BODY T3 IS
          BEGIN
               NULL;
          END T3;

          TASK TYPE T5;

          TASK TASK_2 IS
               FOR T5'STORAGE_SIZE USE $TASK_STORAGE_SIZE; -- ERROR:
          END TASK_2;

          TASK BODY TASK_2 IS
          BEGIN
               NULL;
          END TASK_2;

          TASK BODY T5 IS
          BEGIN
               NULL;
          END T5;

     BEGIN
          NULL;
     END;
END BD2C03A;
