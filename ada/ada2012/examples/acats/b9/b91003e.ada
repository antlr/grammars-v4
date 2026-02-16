-- B91003E.ADA

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
--     CHECK THAT A (MATCHING) TASK BODY IS REQUIRED FOR EACH
--     TASK SPECIFICATION.  IN PARTICULAR, CHECK THAT THE CLOSING
--     IDENTIFIER OF THE TASK BODY MUST MATCH THE IDENTIFIER GIVEN
--     IN THE SPECIFICATIONS.

-- HISTORY:
--     DWC 09/22/87  CREATED ORIGINAL TEST FROM SPLIT OF B91003B.ADA.

PROCEDURE B91003E IS

     TASK T4;
     TASK T5;

     TASK BODY T4 IS
     BEGIN
          NULL;
     END T5;        -- ERROR: WRONG CLOSING IDENTIFIER.

     TASK BODY T5 IS
     BEGIN
          NULL;
     END T4;        -- ERROR: WRONG CLOSING IDENTIFIER.

BEGIN

     NULL;

END B91003E;
