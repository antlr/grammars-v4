-- BE3301C.ADA

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
--     CHECK THAT SET_LINE_LENGTH AND SET_PAGE_LENGTH DO NOT TAKE
--     PARAMETERS OF TYPE INTEGER.  ALSO CHECK THAT LINE_LENGTH AND
--     PAGE_LENGTH RETURN VALUES OF TYPE COUNT.

-- HISTORY:
--     JLH 08/27/87  CREATED ORIGINAL TEST.

WITH TEXT_IO; USE TEXT_IO;

PROCEDURE BE3301C IS

     TWO : CONSTANT INTEGER := 2;
     TEN : CONSTANT INTEGER := 10;
     ITEM : INTEGER;

BEGIN

     SET_LINE_LENGTH (TWO);    -- ERROR: PARAMETER OF TYPE INTEGER.

     NULL;

     SET_PAGE_LENGTH (TEN);    -- ERROR: PARAMETER OF TYPE INTEGER.

     NULL;

     ITEM := LINE_LENGTH;      -- ERROR: TYPES DO NOT MATCH.

     NULL;

     ITEM := PAGE_LENGTH;      -- ERROR: TYPES DO NOT MATCH.

END BE3301C;
