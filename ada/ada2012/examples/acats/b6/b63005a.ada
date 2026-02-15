-- B63005A.ADA

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
--    CHECK THAT IF A SUBPROGRAM SPECIFICATION IS PROVIDED IN A PACKAGE
--    SPECIFICATION, A CORRESPONDING SUBPROGRAM BODY MUST BE
--    PROVIDED IN A PACKAGE BODY.
--
-- PASS/FAIL CRITERIA:
--    The test contains several lines marked POSSIBLE ERROR: [Setn].
--    For each value of n, the implementation must detect one or more of
--    these possible errors. For instance, an error must be detected on
--    at least one of the lines labeled POSSIBLE ERROR: [Set1] for an
--    implementation to pass.
--
-- CHANGE HISTORY:
--    RM  05/05/81
--    RLB 02/03/17  Added additional error tags so reasonable error
--                  reporting strategies are directly supported.


PROCEDURE  B63005A  IS
BEGIN


     DECLARE

          PACKAGE  PACK1  IS
               PROCEDURE  PR1
                 (A1 : INTEGER; A2 : BOOLEAN); -- POSSIBLE ERROR: [Set1] {1:16}
          END  PACK1;

          PACKAGE BODY  PACK1  IS
          END  PACK1;     -- POSSIBLE ERROR: [Set1] {1:11} Body of PR1 missing.

     BEGIN
          NULL;
     END;


     DECLARE

          PACKAGE  PACK1  IS
               FUNCTION  FN1(A1 : INTEGER; A2 : BOOLEAN; A3 : CHARACTER)
                         RETURN INTEGER;       -- POSSIBLE ERROR: [Set2] {1:16}
          END  PACK1;

          PACKAGE BODY  PACK1  IS
          END  PACK1;     -- POSSIBLE ERROR: [Set2] {1:11} Body of FN1 missing.

     BEGIN
          NULL;
     END;


     DECLARE

          PACKAGE  PACK1  IS
               PROCEDURE  PR1
                 (A1 : INTEGER; A2 : BOOLEAN); -- POSSIBLE ERROR: [Set3] {1:16}
          END  PACK1;

          PACKAGE BODY  PACK1  IS
               FUNCTION   PR1(A1 : INTEGER; A2 : BOOLEAN)
                          RETURN INTEGER   IS
               BEGIN
                    RETURN 17;
               END  PR1;

          END  PACK1;     -- POSSIBLE ERROR: [Set3] {1:11} Body of PR1 missing.

     BEGIN
          NULL;
     END;


     DECLARE

          PACKAGE  PACK1  IS
               FUNCTION   FN1(A1 : INTEGER; A2 : BOOLEAN)
                          RETURN INTEGER;      -- POSSIBLE ERROR: [Set4] {1:16}
          END  PACK1;

          PACKAGE BODY  PACK1  IS
               PROCEDURE  FN1(A1 : INTEGER; A2 : BOOLEAN)
                          IS
               BEGIN
                    NULL;
               END  FN1;

          END  PACK1;     -- POSSIBLE ERROR: [Set4] {1:11} Body of FN1 missing.


     BEGIN
          NULL;
     END;


     DECLARE

          PACKAGE  PACK1  IS
               FUNCTION  "+"(A1 : INTEGER; A2 : CHARACTER)
                         RETURN CHARACTER;     -- POSSIBLE ERROR: [Set5] {1:16}
          END  PACK1;

          PACKAGE BODY  PACK1  IS
          END  PACK1;     -- POSSIBLE ERROR: [Set4] {1:11} Body of "+" missing.

     BEGIN
          NULL;
     END;


     DECLARE

          PACKAGE  PACK1  IS

               PACKAGE  PACK2  IS
                    I  :  INTEGER;
               END  PACK2;

               FUNCTION  "+"(A1 : INTEGER; A2 : CHARACTER)
                         RETURN CHARACTER ;    -- POSSIBLE ERROR: [Set6] {1:16}

          END  PACK1;

          PACKAGE BODY  PACK1  IS
               PACKAGE BODY  PACK2  IS
                    FUNCTION  "+"(A1 : INTEGER; A2 : CHARACTER)
                              RETURN CHARACTER  IS
                    BEGIN
                         RETURN  A2;
                    END  "+";
               END  PACK2;

          END  PACK1;    -- POSSIBLE ERROR: [Set6] {1:11}
                         -- Body of PACK1."+" missing.

     BEGIN
          NULL;
     END;


END B63005A;
