-- B22001M.TST

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
-- CHECK THAT A FIXED POINT REAL LITERAL CANNOT BE
--  CONTINUED ACROSS A LINE BOUNDARY.

-- FOR IMPLEMENTATIONS THAT USE FIXED LENGTH INPUT LINES,
-- ADDITIONAL BLANKS MUST NOT BE ADDED TO THE END OF THOSE LINES
-- THAT TRY TO FORCE A LEXICAL TOKEN ACROSS A LINE BOUNDARY.
-- THUS, SUFFICIENT (I.E., MAX_IN_LEN - 20) BLANKS ARE MACRO EXPANDED
-- AT THE START OF THOSE PARTICULAR LINES SO AS TO BRING THE
-- LINE LENGTH UP TO THE MAXIMUM ALLOWED INPUT LINE LENGTH.

-- JRK 4/21/80
-- JRK 12/16/80

PROCEDURE B22001M IS

     TYPE FIXED IS DELTA 0.01 RANGE 0.0 .. 10.0;

     F1 : FIXED;

BEGIN

$BLANKS           F1 := 0.5
E1;                 -- ERROR: REAL LITERAL CROSSES LINE BOUNDARY.
     NULL;

END B22001M;
