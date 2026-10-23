-- BD5005D.ADA

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
--     CHECK THAT AN ADDRESS CLAUSE CANNOT BE GIVEN, IMMEDIATELY WITHIN
--     THE SPECIFICATION OF TASK T, FOR AN EXPANDED NAME OF THE FORM
--     T.E, WHERE E IS AN ENTRY OF T.

-- HISTORY:
--     JET 08/23/88  CREATED ORIGINAL TEST.

WITH SYSTEM, SPPRT13;
PROCEDURE BD5005D IS

     TASK T IS
          ENTRY E;
          FOR T.E USE AT SPPRT13.ENTRY_ADDRESS;  -- ERROR: NOT ALLOWED.
     END T;

     TASK BODY T IS
     BEGIN
          ACCEPT E DO
               NULL;
          END E;
     END T;

BEGIN
     NULL;
END BD5005D;
