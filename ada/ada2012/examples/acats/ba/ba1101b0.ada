-- BA1101B0M.ADA

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
-- CHECK THAT A SUBUNIT OR NESTED PACKAGE CANNOT BE
--    NAMED IN A WITH_CLAUSE.

-- SEPARATE FILES ARE:
--   BA1101B0M THE MAIN PROCEDURE.
--   BA1101B1  A SUBUNIT PACKAGE BODY.
--   BA1101B2  A SUBUNIT PROCEDURE BODY.
--   BA1101B3  A LIBRARY PACKAGE.
--   BA1101B4  A LIBRARY PROCEDURE.

-- WKB 6/19/81

PROCEDURE BA1101B0M IS

     PACKAGE BA1101B1 IS
          I : INTEGER;
     END BA1101B1;

     PACKAGE BODY BA1101B1 IS SEPARATE;

     PROCEDURE BA1101B2 IS SEPARATE;

BEGIN
     BA1101B1.I := 1;
END BA1101B0M;
