-- BE3205A.ADA

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
-- CHECK THAT THE STANDARD INPUT AND OUTPUT FILES CANNOT BE
-- OPENED, CLOSED, RESET, OR DELETED.

-- ABW  8/25/82
-- SPS  11/9/82
-- JBG 3/17/83

WITH TEXT_IO;
USE TEXT_IO;
PROCEDURE BE3205A IS
BEGIN

     OPEN (STANDARD_INPUT, IN_FILE, "X3205A");    -- ERROR: OPEN.
     OPEN (STANDARD_OUTPUT, OUT_FILE, "X3205B");  -- ERROR: OPEN.

     RESET (STANDARD_INPUT);                      -- ERROR: RESET.
     RESET (STANDARD_OUTPUT);                     -- ERROR: RESET.

     CLOSE (STANDARD_INPUT);                      -- ERROR: CLOSE.
     CLOSE (STANDARD_OUTPUT);                     -- ERROR: CLOSE.

     DELETE (STANDARD_INPUT);                     -- ERROR: DELETE.
     DELETE (STANDARD_OUTPUT);                    -- ERROR: DELETE.

END BE3205A;
