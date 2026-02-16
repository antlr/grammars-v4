-- B26001A.ADA

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
-- CHECK THAT THE STRING DELIMITERS QUOTE AND PERCENT CANNOT BE
-- INTERMIXED; THAT IS, WHICHEVER IS USED AS THE OPENING DELIMITER
-- MUST ALSO BE USED AS THE CLOSING DELIMITER.

-- PWB  02/21/86

PROCEDURE B26001A IS

     S3A : STRING (1..3) := "ABC%;           -- ERROR: QUOTE EXPECTED.
     I1  : INTEGER;
     S3B : STRING (1..3) := %ABC";           -- ERROR: PERCENT EXPECTED.
     I2  : INTEGER;
     VAR2 : STRING (1..2);
     VAR3 : STRING (1..3);

BEGIN

     VAR3 := "ABC%;                          -- ERROR: QUOTE EXPECTED.
     NULL;
     VAR2 := %BC";                           -- ERROR: PERCENT EXPECTED.
     NULL;
     VAR3 := "A% & VAR2;                     -- ERROR: QUOTE EXPECTED.
     NULL;
     VAR3 := 'X' & %AB";                     -- ERROR: PERCENT EXPECTED.
     NULL;

END B26001A;
