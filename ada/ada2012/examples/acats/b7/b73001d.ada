-- B73001D.ADA

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
--
-- OBJECTIVE:
--    CHECK THAT IF A PACKAGE SPECIFICATION REQUIRES A BODY, AND THE
--    PACKAGE IS DECLARED INSIDE ANOTHER PACKAGE SPECIFICATION,
--    BODIES MUST BE PROVIDED FOR BOTH SPECIFICATIONS.
--
-- PASS/FAIL CRITERIA:
--    The test contains several lines marked POSSIBLE ERROR: [Setn].
--    For each value of n, the implementation must detect one or more of
--    these possible errors. For instance, an error must be detected on
--    at least one of the lines labeled POSSIBLE ERROR: [Set1] for an
--    implementation to pass.
--
-- CHANGE HISTORY:
--    RM  05/06/81
--    JBG 09/19/83
--    RLB 02/06/17  Added additional error tags so reasonable error
--                  reporting strategies are directly supported.
--
--!

PROCEDURE  B73001D  IS
BEGIN


     DECLARE

          PACKAGE  PACK1  IS

               PACKAGE  PACK2  IS
                    I  :  INTEGER;
                    FUNCTION  FN1( A1 : INTEGER;  A2 : BOOLEAN )
                              RETURN INTEGER;
               END  PACK2;         -- POSSIBLE ERROR: [Set1] {4:16}

          END  PACK1;

          PACKAGE BODY  PACK1  IS

               FUNCTION  FN1( A1 : INTEGER;  A2 : BOOLEAN )
                         RETURN INTEGER  IS
               BEGIN
                    RETURN  A1;
               END  FN1;

          END  PACK1;   -- POSSIBLE ERROR: [Set1] {2:24} BODY OF PACK2 MISSING.

     BEGIN
          NULL;
     END;


     DECLARE

          PACKAGE  PACK1  IS

               PACKAGE  PACK2  IS
                    I  :  INTEGER;
                    FUNCTION  "+"( A1 : INTEGER;  A2 : CHARACTER )
                              RETURN CHARACTER ;
               END  PACK2;

          END  PACK1;

     BEGIN                -- ERROR: {10:11} BOTH BODIES MISSING.

          NULL;
     END;

     DECLARE

          PACKAGE PACK3 IS

               PACKAGE IN3 IS
                    TASK T;
               END IN3;

          END PACK3;

     BEGIN              -- ERROR: {8:11} BODY OF PACK3 MISSING.
          NULL;
     END;

     DECLARE

          PACKAGE PACK4 IS
               PACKAGE IN2 IS
               PRIVATE
                    TYPE INC;
               END IN2;
          END PACK4;

     BEGIN          -- ERROR: {8:11} BODY OF PACK4 MISSING.
          NULL;
     END;

END B73001D;
