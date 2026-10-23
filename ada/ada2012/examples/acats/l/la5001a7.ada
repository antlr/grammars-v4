-- LA5001A7M.ADA

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
-- CHECK THAT A MAIN PROGRAM CANNOT BE INVOKED IF ITS
--   ELABORATION REQUIRES AN INCONSISTENT ELABORATION ORDER
--   OF LIBRARY UNITS DUE TO PRAGMA ELABORATE.  THE CIRCULARITY CAN BE
--   DETECTED EITHER AT COMPILE TIME (IN LA5001A6) OR AT LINK TIME.  IN
--   ANY CASE, NO LIBRARY UNIT MAY BE ELABORATED AT RUN TIME.

-- SEPARATE FILES ARE:
--   LA5001A0  A LIBRARY PACKAGE.
--   LA5001A1  A LIBRARY PACKAGE DECLARATION.
--   LA5001A2  A LIBRARY PACKAGE DECLARATION.
--   LA5001A3  A LIBRARY PACKAGE DECLARATION.
--   LA5001A4  A LIBRARY PACKAGE BODY (LA5001A1).
--   LA5001A5  A LIBRARY PACKAGE BODY (LA5001A2).
--   LA5001A6  A LIBRARY PACKAGE BODY (LA5001A3).
--   LA5001A7M THE MAIN PROCEDURE.

-- WKB 7/16/81
-- JBG 6/4/84
-- JRK 9/24/84

WITH LA5001A2;
WITH REPORT; USE REPORT;
PROCEDURE LA5001A7M IS

BEGIN
     -- PROCEDURE TEST INVOKED IN LA5001A0.

     FAILED ("MAIN PROCEDURE LA5001A7M EXECUTED");

     RESULT;
END LA5001A7M;
