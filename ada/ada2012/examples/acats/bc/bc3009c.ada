-- BC3009C.ADA

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
-- CHECK THAT A GENERIC UNIT MAY NOT REQUIRE AN INSTANTIATION
-- OF ITSELF. (NOTE: INSTANTIATION OF SELF IS IMPOSSIBLE IN ADA 83.)

-- RFB 05/24/84
-- EG  05/31/84
-- JRL 03/20/92  CONSOLIDATED WITH BC3009A.
-- JRL 09/04/92  REMOVED INSTANTIATION FROM BC3009C SPEC TO PREVENT
--               POSSIBLE REJECTION OF ENTIRE SPEC PER AI-00506.
-- RLB 07/20/02  SPLIT INTO B AND L TESTS, AS THIS IS A POST-COMPILATION RULE.
--               B-TEST ONLY CONTAINS DIRECT INSTANTIATION OF SELF, WHICH IS
--               ILLEGAL BECAUSE SELF IS NOT GENERIC INSIDE THE GENERIC UNIT.
--
-- UNIT 1
--
PACKAGE BC3009C IS

     GENERIC
     PROCEDURE F;

END BC3009C;

--
-- UNIT 2
--
PACKAGE BODY BC3009C IS

     GENERIC
     PACKAGE T IS
          PACKAGE NT IS NEW T;     -- ERROR: T DENOTES NON-GENERIC CURRENT
                                   -- INSTANCE, NOT THE GENERIC UNIT T
                                   -- (USER ATTEMPTED INSTANTIATION OF "SELF")
     END;

     PROCEDURE F IS
     BEGIN
          NULL;
     END;

BEGIN

     NULL;

END BC3009C;
