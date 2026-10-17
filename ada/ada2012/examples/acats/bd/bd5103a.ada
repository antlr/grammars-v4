-- BD5103A.ADA

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
--     CHECK THAT THE NAME IN AN ADDRESS CLAUSE FOR AN ENTRY CANNOT
--     BE AN EXPANDED NAME.

-- HISTORY:
--     DJ  09/08/87  CREATED ORIGINAL TEST.
--     DWC 09/29/87  REFORMATTED TEST.

WITH SYSTEM;
WITH SPPRT13; USE SPPRT13;

PACKAGE BD5103A IS

     TASK TASK1 IS
          ENTRY ENTRY1;
     END TASK1;

     TASK TASK2 IS
          ENTRY ENTRY1;
     END TASK2;

     FOR TASK1.ENTRY1 USE AT ENTRY_ADDRESS;    -- ERROR:  EXPANDED NAME.

END BD5103A;
