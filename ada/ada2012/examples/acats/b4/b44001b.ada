-- B44001B.ADA

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
--     CHECK THE DETECTION OF SYNTAX ERRORS IN EXPRESSIONS.

--     THIS TEST CHECKS THAT A PROCEDURE CALL CANNOT BE A PRIMARY.

-- HISTORY:
--     DWC 09/22/87  CREATED ORIGINAL TEST FROM SPLIT OF B44001A.ADA.

PROCEDURE  B44001B  IS

     A , B , C , D , E : BOOLEAN:= TRUE;
     I , J , K         : INTEGER := 0;

     PROCEDURE  PROC1( X : INTEGER := 17 )  IS
     BEGIN
          NULL;
     END PROC1;

     PROCEDURE  PROC2  IS
     BEGIN
          NULL;
     END PROC2;

BEGIN

     -- A <PROCEDURE_CALL> CANNOT BE A <PRIMARY>.

     I  :=  J ** PROC1(K);    -- ERROR: (E)
     NULL;
     I  :=  J ** PROC1   ;    -- ERROR: (E)
     NULL;
     I  :=  J ** PROC2   ;    -- ERROR: (E)
     NULL;

END  B44001B;
