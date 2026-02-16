-- B91003D.ADA

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
--     CHECK THAT A TASK SPECIFICATION CANNOT FOLLOW THE TASK BODY IN A
--     DECLARATIVE PART.

-- PASS/FAIL CRITERIA:
--     The test contains several lines marked POSSIBLE ERROR: [Setn].
--     For each value of n, the implementation must detect one or more of
--     these possible errors. For instance, an error must be detected on
--     at least one of the lines labeled POSSIBLE ERROR: [Set1] for an
--     implementation to pass.
--
--     Note: We allow errors on the (late) declarations in this test so
--     that error correction strategies work; if that declaration is treated
--     separately from the (early) bodies, it could be reported as an
--     illegal homograph or as missing a body.
--
-- CHANGE HISTORY:
--     TBN 01/30/86
--     RLB 02/03/17  Added additional error tags so reasonable error
--                   reporting strategies are directly supported.

PROCEDURE B91003D IS

     TASK BODY FRED IS       -- POSSIBLE ERROR: [Set1] {6}  Body precedes spec.

     BEGIN
          NULL;
     END FRED;

     TASK FRED;              -- POSSIBLE ERROR: [Set1] {6}

     TASK BODY T1 IS         -- POSSIBLE ERROR: [Set2] {6}  Body precedes spec.

     BEGIN
          ACCEPT E;          -- OPTIONAL ERROR: {11} E undeclared.
     END T1;

     TASK TYPE T1 IS         -- POSSIBLE ERROR: [Set2] {6}
          ENTRY E;
     END T1;

BEGIN
     NULL;
END B91003D;
