-- B91003B.ADA

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
--     CHECK THAT A (MATCHING) TASK BODY IS REQUIRED FOR EACH
--     TASK SPECIFICATION.
--
-- PASS/FAIL CRITERIA:
--     The test contains several lines marked POSSIBLE ERROR: [Setn].
--     For each value of n, the implementation must detect one or more of
--     these possible errors. For instance, an error must be detected on
--     at least one of the lines labeled POSSIBLE ERROR: [Set1] for an
--     implementation to pass.
--
-- CHANGE HISTORY:
--     WEI 03/04/82
--     RJK 02/01/84  ADDED TO ACVC
--     JWC 06/28/85  FROM B910ABA-B.ADA
--     DWC 09/22/87  MOVED CHECK THAT THE CLOSING IDENTIFIER MUST
--                   MATCH THE IDENTIFIER GIVEN IN THE SPECIFICATION
--                   TO B91003E.ADA.
--     RLB 02/03/17  Added additional error tags so reasonable error
--                   reporting strategies are directly supported.

PROCEDURE B91003B IS

     TASK T1;          -- OK. {6}
     TASK T2;          -- POSSIBLE ERROR: [Set3] {6}
     TASK T3;          -- POSSIBLE ERROR: [Set3] {6}

     TASK BODY T1 IS
     BEGIN
          NULL;
     END T1;           -- OK. {3:6}

     TASK BODY T22 IS  -- POSSIBLE ERROR: [Set1] {6}
     BEGIN
          NULL;
     END T22;          -- POSSIBLE ERROR: [Set1] {6}
                       -- Body without specification.

     TASK TYPE TT1;    -- OK. {6}
     TASK TYPE TT2;    -- POSSIBLE ERROR: [Set3] {6}
     TASK TYPE TT3;    -- POSSIBLE ERROR: [Set3] {6}

     TASK BODY TT1 IS
     BEGIN
          NULL;
     END TT1;          -- OK. {3:6}

     TASK BODY TT22 IS -- POSSIBLE ERROR: [Set2] {6}
     BEGIN
          NULL;
     END TT22;         -- POSSIBLE ERROR: [Set2] {6}
                       -- Body without specification.

-- Missing body for tasks T2,T3;
-- Missing body for task types TT2, TT3.

BEGIN                  -- POSSIBLE ERROR: [Set3] {4:1}
     NULL;
END B91003B;
