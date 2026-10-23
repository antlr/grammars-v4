-- BA1101C2M.ADA

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
-- CHECK THAT A LIBRARY UNIT, P, CANNOT BE NAMED IN
--   A WITH_CLAUSE AS STANDARD.P, NOR CAN IT BE NAMED IN A USE_CLAUSE
--   WITHOUT A PRECEDING WITH_CLAUSE.

-- SEPARATE FILES ARE:
--   BA1101C0  A LIBRARY PACKAGE.
--   BA1101C1  A LIBRARY PACKAGE.
--   BA1101C2M THE MAIN PROCEDURE.
--   BA1101C3  A LIBRARY PACKAGE.
--   BA1101C4  A LIBRARY PACKAGE BODY.
--   BA1101C5  A LIBRARY PACKAGE BODY.
--   BA1101C6  A SUBUNIT PROCEDURE.

-- WKB 6/19/81
-- BHS 7/19/84
-- PWN 12/04/95  SPLIT TEST INTO MORE SEPARATE FILES.

WITH STANDARD.BA1101C0;      -- ERROR: 'STANDARD.' USED IN WITH_CLAUSE.
PROCEDURE BA1101C2M IS

BEGIN

     NULL;

END BA1101C2M;
