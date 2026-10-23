-- B91003C.ADA

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
--     CHECK THAT A TASK SPECIFICATION AND THE CORRESPONDING BODY MUST
--     OCCUR IN THE SAME DECLARATIVE PART.
--
-- PASS/FAIL CRITERIA:
--     The test contains several lines marked POSSIBLE ERROR: [Setn].
--     For each value of n, the implementation must detect one or more of
--     these possible errors. For instance, an error must be detected on
--     at least one of the lines labeled POSSIBLE ERROR: [Set1] for an
--     implementation to pass.
--
-- CHANGE HISTORY:
--     WEI  3/ 4/82
--     RJK  2/ 1/84   ADDED TO ACVC
--     JWC  6/28/85   RENAMED FROM B910AEA-B.ADA
--     RLB  2/03/17   Added additional error tags so reasonable error
--                    reporting strategies are directly supported.


PROCEDURE B91003C IS

     TASK T1;              -- POSSIBLE ERROR: [Set1] {6}
     TASK T5;              -- POSSIBLE ERROR: [Set1] {6}
     TASK T6;              -- POSSIBLE ERROR: [Set1] {6}
     TASK T8;              -- POSSIBLE ERROR: [Set1] {6}

     PACKAGE PACK IS
          TASK T2;
     END PACK;

     PACKAGE BODY PACK IS
          TASK T3;         -- POSSIBLE ERROR: [Set2] {11} No task body here.

          TASK BODY T2 IS
          BEGIN
               NULL;
          END T2;

          TASK BODY T1 IS
          BEGIN
               NULL;
          END T1;          -- ERROR: {3:11} Body not in same Decl. part.

     END PACK;             -- POSSIBLE ERROR: [Set2] {1:1}  No body for T3.

     PROCEDURE PROC IS
          TASK T4;         -- POSSIBLE ERROR: [Set3] {11} No task body here.

          TASK BODY T3 IS
          BEGIN
               NULL;
          END T3;          -- ERROR: {3:11} Specifation not in same Decl. part.

          TASK BODY T5 IS
          BEGIN
               NULL;
          END T5;          -- ERROR: {3:11} Specifation not in same Decl. part.

     BEGIN                 -- POSSIBLE ERROR: [Set3] {1:1} No body for T4.
          NULL;
     END PROC;

BEGIN                      -- POSSIBLE ERROR: [Set1] {1:1}
                           -- No bodies for T1, T5, T6, T8.

BLOCK1 :
     DECLARE
          TASK T7;         -- POSSIBLE ERROR: [Set4] {11} No body given here.

          TASK BODY T6 IS
          BEGIN
               NULL;
          END T6;          -- ERROR: {3:11} Specifation not in same Decl. part.

          TASK BODY T4 IS
          BEGIN
               NULL;
          END T4;          -- ERROR: {3:11} Specifation not in same Decl. part.

     BEGIN                 -- POSSIBLE ERROR: [Set4] {1:1} No body for T7.
          NULL;
     END BLOCK1;

BLOCK2 :
     DECLARE
          TASK T13 IS
               ENTRY E;
          END T13;

          TASK BODY T7 IS
          BEGIN
               NULL;
          END T7;          -- ERROR: {3:11} Specifation not in same Decl. part.

          TASK BODY T8 IS
          BEGIN
               NULL;
          END T8;          -- ERROR: {3:11} Specifation not in same Decl. part.

          TASK BODY T13 IS
          BEGIN
               ACCEPT E;
          END T13;         -- OK. {3:11} IN SPITE OF 13.

     BEGIN
          T13.E;           -- OK. {11} IF TASK 13 IS RECOGNIZED.
     END BLOCK2;

END B91003C;
