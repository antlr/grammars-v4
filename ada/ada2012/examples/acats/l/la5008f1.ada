-- LA5008F1M.ADA

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
-- CHECK THAT EXECUTION OF A MAIN PROGRAM IS NOT ALLOWED IF A NEEDED
--   GENERIC UNIT IS MISSING OR OBSOLETE.

-- CASE F:  MISSING SUBUNIT GENERIC PACKAGE BODY (NON-GENERIC PARENT).

-- SEPARATE FILES ARE:
--   LA5008F0  A LIBRARY PACKAGE DECL AND BODY.
--   LA5008F1M THE MAIN PROCEDURE.

-- EXPECT A LINK-TIME ERROR MESSAGE THAT THE BODY OF GENERIC PACKAGE
--   LA5008F0.LA5008F0P IS MISSING.
-- ALTERNATIVELY, EXPECT A COMPILE-TIME ERROR MESSAGE FOR FILE LA5008F0
--   THAT THE BODY OF GENERIC PACKAGE LA5008F0.LA5008F0P IS NOT IN THE
--   SAME COMPILATION FILE AS ITS DECLARATION.

-- JRK 11/1/85

WITH REPORT; USE REPORT;
WITH LA5008F0;

PROCEDURE LA5008F1M IS
BEGIN
     TEST ("LA5008F", "CHECK THAT MAIN PROGRAM NOT EXECUTED IF A " &
                      "SUBUNIT GENERIC PACKAGE BODY IS MISSING");

     FAILED ("SHOULD NOT EXECUTE");

     RESULT;
END LA5008F1M;
