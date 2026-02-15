-- B62001D.ADA

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
-- CHECK THAT FUNCTIONS CAN NEITHER BE DECLARED NOR PASSED AS SUBPROGRAM
-- PARAMETERS.

-- DAS 1/23/81
-- JRK 3/16/81
-- JBG 8/21/83

PROCEDURE B62001D IS

     PROCEDURE P11 (F : FUNCTION RETURN INTEGER) IS -- ERROR: FORMAL
                                                    -- FUNC PARAM.
          FUNCTION FF RETURN INTEGER IS
          BEGIN
               RETURN 0;
          END FF;
     BEGIN
          P11 (FF);           -- OPTIONAL ERROR: ACTUAL FUNCTION PARAMETER.
     END P11;

BEGIN
     NULL;
END B62001D;
