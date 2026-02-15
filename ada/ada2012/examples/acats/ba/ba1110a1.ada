-- BA1110A1M.ADA

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
-- CHECK THAT A USE CLAUSE FOR A SUBUNIT CANNOT NAME A PACKAGE THAT IS
-- ONLY NAMED IN A WITH CLAUSE OF AN ANCESTOR UNIT.

-- THE FOLLOWING FILES DEPEND ON EACH OTHER:
--           BA1110A0  - A PACKAGE.
--           BA1110A1M - THE MAIN PROCEDURE.
--           BA1110A2  - A SEPARATE PACKAGE SUBUNIT.
--           BA1110A3  - A SEPARATE PROCEDURE SUBUNIT.
--           BA1110A4  - A SEPARATE FUNCTION SUBUNIT.
--           BA1110A5  - A SEPARATE TASK SUBUNIT.
-- THIS UNIT SHOULD COMPILE CORRECTLY.

-- TBN 2/10/86

WITH BA1110A0;
PROCEDURE BA1110A1M IS

     PACKAGE BA1110A2 IS
     END BA1110A2;

     PACKAGE BODY BA1110A2 IS SEPARATE;

     PROCEDURE BA1110A3 IS SEPARATE;

     FUNCTION BA1110A4 RETURN BOOLEAN IS SEPARATE;

     TASK BA1110A5;

     TASK BODY BA1110A5 IS SEPARATE;

-------------------------------------------

BEGIN          -- (MAIN)
     NULL;
END BA1110A1M;
