-- BA1001A0M.ADA

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
--     CHECK THAT A SUBPROGRAM CANNOT BE COMPILED AS A LIBRARY UNIT OR
--     SUBUNIT IF ITS DESIGNATOR IS AN OPERATOR SYMBOL.

-- SEPARATE FILES ARE:
--     BA1001A0M - CONTAINS THE LIMITED TYPE 'LIM'.  THIS FILE SHOULD
--                 COMPILE WITHOUT ERRORS.
--     BA1001A1  - CONTAINS OPERATOR FUNCTION AS LIBRARY UNIT.
--     BA1001A4  - CONTAINS OPERATOR FUNCTION AS LIBRARY UNIT.
--     BA1001AC  - CONTAINS OPERATOR FUNCTION AS LIBRARY UNIT.

-- HISTORY:
--     JET 003/25/88  CREATED ORIGINAL TEST.
--     THS 04/103/90  SPLIT TEST TO BA1001A*.ADA AND BA1001D.ADA.
--     DTN 003/15/93  ELIMINATE FILES FROM PROLOGUE THAT WERE DELETED FOR 
--                    9X BASIC.

PACKAGE BA1001A0M_PKG IS
     TYPE LIM IS LIMITED PRIVATE;
PRIVATE
     TYPE LIM IS RANGE -10..10;
END BA1001A0M_PKG;

PROCEDURE BA1001A0M IS
BEGIN
     NULL;
END BA1001A0M;
