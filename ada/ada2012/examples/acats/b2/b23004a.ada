-- B23004A.ADA

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
-- CHECK THAT # ' ARE NOT ALLOWED IN IDENTIFIERS.

-- DCB 12/19/79
-- JRK 10/23/80
-- JWC 6/28/85   RENAMED TO -AB

PROCEDURE B23004A IS

     I7   : INTEGER;
     I#8  : INTEGER;   -- ERROR: # IN MIDDLE
     I9   : INTEGER;
     I'10 : INTEGER;   -- ERROR: ' IN MIDDLE
     I13  : INTEGER;
     #I14 : INTEGER;   -- ERROR: # LEADING
     I14B : INTEGER;
     'I14 : INTEGER;   -- ERROR: ' LEADING
     I21  : INTEGER;
     I22# : INTEGER;   -- ERROR: # TRAILING
     I23  : INTEGER;
     I24' : INTEGER;   -- ERROR: ' TRAILING
     I25  : INTEGER;

BEGIN

     NULL;
     I#8  := 0;     -- ERROR: # IN MIDDLE
     NULL;
     I'10 := 0;     -- ERROR: ' IN MIDDLE
     NULL;

END B23004A;
