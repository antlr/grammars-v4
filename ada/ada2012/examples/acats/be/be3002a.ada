-- BE3002A.ADA

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
-- CHECK THAT FILE_MODE IS VISIBLE AND HAS LITERAL IN_FILE AND OUT_FILE,
-- NOT INOUT_FILE.

-- CHECK THAT FILE_TYPE IS LIMITED PRIVATE.

-- SPS 9/30/82
-- SPS 11/9/82
-- JBG 3/16/83

WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE BE3002A IS

     FT   : FILE_TYPE;
     MODE : FILE_MODE;
     FT2  : FILE_TYPE;

BEGIN

     FT2 := FT;                              -- ERROR: LIMITED PRIVATE.

     MODE := IN_FILE;                        -- OK.
     MODE := OUT_FILE;                       -- OK.
     MODE := INOUT_FILE;                     -- ERROR: INOUT_FILE.

     CREATE (FT, IN_FILE);                   -- OK.
     CREATE (FT, OUT_FILE);                  -- OK.
     CREATE (FT, INOUT_FILE);                -- ERROR: INOUT_FILE.

     OPEN (FT, IN_FILE, "X3002A");           -- OK.
     OPEN (FT, OUT_FILE, "X3002A");          -- OK.
     OPEN (FT, INOUT_FILE, "X3002A");        -- ERROR: INOUT_FILE.

     RESET (FT, IN_FILE);                    -- OK.
     RESET (FT, OUT_FILE);                   -- OK.
     RESET (FT, INOUT_FILE);                 -- ERROR: INOUT_FILE.

END BE3002A;
