-- B56001A.ADA

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
-- CHECK THAT A NAMED BLOCK CANNOT BE CLOSED WITHOUT MATCHING END ID.

-- DAT 3/30/81
-- SPS 2/10/83
-- SPS 3/4/83
-- RLB 11/21/19  Added error location indicators.

PROCEDURE B56001A IS
BEGIN

     B1 : BEGIN
          NULL;
     END;                                    -- ERROR: MISSING B1.  {2:6;1}

     B2 : BEGIN
          NULL;
     END B1;                                 -- ERROR: B1 NOT B2.   {2:6;1}

     B3 : DECLARE
     BEGIN
          NULL;
     EXCEPTION
          WHEN OTHERS => NULL;
     END B3;                                 -- OK.                 {5:6;1}

     L1 : DECLARE
          BEGIN
               NULL;
          END L2;                            -- ERROR: L2.          {3:6;1}

     L3 : DECLARE
          BEGIN NULL;
          END;                               -- ERROR: MISSING ID.  {2:6;1}

END B56001A;
