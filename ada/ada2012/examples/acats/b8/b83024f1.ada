-- B83024F1.ADA

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
--     SEE B83024F0M.ADA.

-- TEST FILES:
--      THIS TEST CONSISTS OF THE FOLLOWING FILES:
--         B83024F0M.ADA - (THIS FILE) MAIN PROGRAM.
--      -> B83024F1.ADA -- PACKAGE BODY FOR B83024F_P1.
--         B83024F2.ADA -- PACKAGE BODY FOR B83024F_P2.
--         B83024F3.ADA -- PACKAGE BODY FOR B83024F_P3.

-- HISTORY:
--     BCB 08/30/88  CREATED ORIGINAL TEST.
--     LDC 10/10/90  SPLIT PACKAGE BODIES INTO SEPARATE FILES.
--     PWN 12/27/94  ADDED A PROCEDURE TO KEEP PACKAGE BODIES LEGAL.
--     RLB 09/13/99  UPDATED APPLICABILITY CRITERIA AND HEADERS FOR ADA 95.

PACKAGE BODY B83024F_P1 IS
     PACKAGE BODY B83024F_PACK1 IS
          X : INTEGER := F;                                 -- ERROR:
     BEGIN
          NULL;
     END B83024F_PACK1;

     PROCEDURE REQUIRE_BODY IS
     BEGIN
          NULL;
     END;
END B83024F_P1;
