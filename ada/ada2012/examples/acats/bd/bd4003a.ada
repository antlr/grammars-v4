-- BD4003A.ADA

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
--     CHECK THAT A RECORD REPRESENTATION SPECIFICATION CANNOT BE
--     GIVEN:
--     - IN A PACKAGE SPECIFICATION FOR A TYPE DECLARED IN AN INNER
--       PACKAGE SPECIFICATION;
--     - IN A PACKAGE OR TASK SPECIFICATION FOR A TYPE DECLARED IN AN
--       ENCLOSING PACKAGE SPECIFICATION;
--     - IN A PACKAGE BODY FOR A TYPE DECLARED IN THE CORRESPONDING
--       PACKAGE SPECIFICATION.

-- HISTORY:
--     DHH 08/23/88 CREATED ORIGINAL TEST.
--     BCB 03/29/90 SPLIT ORIGINAL TEST INTO BD4003A.TST, BD4003B.TST,
--                  AND BD4003C.TST.
--     THS 09/24/90 RENAMED TEST FROM '.TST' TO '.ADA'. REMOVED MACRO
--                  ALIGNMENT.
--     RLB 06/29/16 MOVED ERROR MARKER TO CORRECT LINE.
PROCEDURE BD4003A IS

     PACKAGE P IS
          PACKAGE INNER_P IS
               TYPE INNER IS
                    RECORD
                         X : BOOLEAN;
                    END RECORD;
          END INNER_P;

          USE INNER_P;

          FOR INNER USE                                       -- ERROR:
               RECORD
                    X AT 0 RANGE 0 .. 0;
               END RECORD;
     END P;

     PACKAGE OUTER_P IS
          TYPE INNER1 IS
               RECORD
                    X : BOOLEAN;
               END RECORD;

          PACKAGE I_P IS
               FOR INNER1 USE                                 -- ERROR:
                    RECORD
                         X AT 0 RANGE 0 .. 0;
                    END RECORD;
          END I_P;
     END OUTER_P;

     PACKAGE OUTER_T IS
          TYPE INNER1_T IS
               RECORD
                    X : BOOLEAN;
               END RECORD;

          TASK I_T IS
               FOR INNER1_T USE                               -- ERROR:
                    RECORD
                         X AT 0 RANGE 0 .. 0;
                    END RECORD;
          END I_T;
     END OUTER_T;

     PACKAGE BODY OUTER_T IS
          TASK BODY I_T IS
          BEGIN
               NULL;
          END;
     BEGIN
          NULL;
     END OUTER_T;

     PACKAGE DIFF_SECT IS
          TYPE INNER3 IS
               RECORD
                    X : BOOLEAN;
               END RECORD;
     END DIFF_SECT;

     PACKAGE BODY DIFF_SECT IS
          FOR INNER3 USE                                      -- ERROR:
               RECORD
                    X AT 0 RANGE 0 .. 0;
               END RECORD;
     BEGIN
          NULL;
     END DIFF_SECT;

BEGIN
     NULL;
END BD4003A;
