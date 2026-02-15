-- LA5001A6.ADA

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
-- WKB 7/16/81
-- JBG 3/8/84
-- JBG 6/4/84
-- JRK 9/24/84

WITH LA5001A1;
WITH LA5001A0;
WITH REPORT; USE REPORT;
PRAGMA ELABORATE (LA5001A1, LA5001A0, REPORT);
PACKAGE BODY LA5001A3 IS     -- OPT. ERROR: CIRCULARITY NOW DETECTABLE.

     FUNCTION H RETURN BOOLEAN IS
     BEGIN
          RETURN FALSE;
     END H;

BEGIN

     FAILED ("LA5001A3 BODY ELABORATED");
     LA5001A1.F;

EXCEPTION
     WHEN PROGRAM_ERROR =>
          FAILED ("PROGRAM_ERROR RAISED IN LA5001A3");
END LA5001A3;
