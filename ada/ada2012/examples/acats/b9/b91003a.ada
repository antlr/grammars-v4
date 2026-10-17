-- B91003A.ADA

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
--     CHECK THAT IF A TASK SPECIFICATION IS PROVIDED IN A PACKAGE
--     SPECIFICATION, A CORRESPONDING TASK BODY MUST BE PROVIDED
--     IN A PACKAGE BODY.
--
-- PASS/FAIL CRITERIA:
--     The test contains several lines marked POSSIBLE ERROR: [Setn].
--     For each value of n, the implementation must detect one or more of
--     these possible errors. For instance, an error must be detected on
--     at least one of the lines labeled POSSIBLE ERROR: [Set1] for an
--     implementation to pass.
--
-- CHANGE HISTORY:
--     RM  05/05/81
--     ABW 06/17/82
--     RLB 02/03/17  Added additional error tags so reasonable error
--                   reporting strategies are directly supported.


PROCEDURE  B91003A  IS
BEGIN


     DECLARE

          PACKAGE  PACK1  IS
               TASK  TK1;          -- POSSIBLE ERROR: [Set1] {16}
          END  PACK1;

          PACKAGE BODY  PACK1  IS
          END  PACK1;              -- POSSIBLE ERROR: [Set1] {1:11}
                                   -- Body of TK1 missing.

     BEGIN
          NULL;
     END;


     DECLARE

          PACKAGE  PACK1  IS
               TASK TYPE  TK1;     -- POSSIBLE ERROR: [Set2] {16}
          END  PACK1;

          PACKAGE BODY  PACK1  IS
          END  PACK1;              -- POSSIBLE ERROR: [Set2] {1:11}
                                   -- Body of TK1 missing.

     BEGIN
          NULL;
     END;


     DECLARE

          PACKAGE  PACK1  IS
               TASK  TK1  IS
                    ENTRY  E1;
               END  TK1;           -- POSSIBLE ERROR: [Set3] {2:16}
          END  PACK1;

          PACKAGE BODY  PACK1  IS
          END  PACK1;              -- POSSIBLE ERROR: [Set3] {1:11}
                                   -- Body of TK1 missing.

     BEGIN
          NULL;
     END;


     DECLARE

          PACKAGE  PACK1  IS
               TASK TYPE  TK1  IS
                    ENTRY  E1;
               END  TK1;           -- POSSIBLE ERROR: [Set4] {2:16}
          END  PACK1;

          PACKAGE BODY  PACK1  IS
          END  PACK1;              -- POSSIBLE ERROR: [Set4] {1:11}
                                   -- Body of TK1 missing.

     BEGIN
          NULL;
     END;


     DECLARE

          PACKAGE  PACK1  IS

               PACKAGE  PACK2  IS
                    I : INTEGER;
               END  PACK2;

               TASK TYPE  TK1  IS
                    ENTRY  E1;
               END  TK1;           -- POSSIBLE ERROR: [Set5] {2:11}

          END  PACK1;

          PACKAGE BODY  PACK1  IS  -- POSSIBLE ERROR: [Set5] {11}
               PACKAGE BODY  PACK2  IS
                    TASK BODY  TK1  IS  -- ERROR: {21} No TK1 in PACK2.
                    BEGIN
                         NULL;
                    END  TK1;
               END  PACK2;
          END  PACK1;              -- POSSIBLE ERROR: [Set5] {11}
                                   -- Body of PACK1.TK1 Missing.

     BEGIN
          NULL;
     END;


END B91003A;
