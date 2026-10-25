-- B52004D.DEP

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
--     CHECK THAT TYPES OF TARGET VARIABLE AND EXPRESSION MUST MATCH
--     AT COMPILE TIME FOR LONG_INTEGER TYPE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     THE TYPE "LONG_INTEGER".
--
--     IF "LONG_INTEGER" IS NOT SUPPORTED, THEN THE DECLARATION OF
--     LI1 MUST BE REJECTED.

-- HISTORY:
--     JRK 07/17/80  CREATED ORIGINAL TEST.
--     SPS 03/21/83
--     BCB 12/28/87  MODIFIED HEADER.

PROCEDURE B52004D IS

     I1 : INTEGER := 6;
     LI1 : LONG_INTEGER := 77;                         -- N/A => ERROR.

     TYPE TA1 IS ARRAY (1..10) OF INTEGER;
     TYPE TA4 IS ARRAY (1..10) OF LONG_INTEGER;

     V1 : TA1 := (1,2,3,4,5,6,7,8,9,0);
     V4 : TA4 := (1..10 => 20);

BEGIN

     I1 := LI1;         -- ERROR: TYPES DON'T MATCH.
     LI1 := I1;         -- ERROR: TYPES DON'T MATCH.

     V4 := V1;          -- ERROR: TYPES DON'T MATCH.

END B52004D;
