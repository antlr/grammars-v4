-- BC1109B.ADA

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
-- CHECK THAT A GENERIC FORMAL OBJECT DECLARATION
--     MAY NOT SPECIFY AN ACCURACY CONSTRAINT.

-- PWB   1/24/86

PROCEDURE BC1109B IS

     TYPE FLOATING IS DIGITS 4 RANGE -1.0 .. 1.0;
     TYPE FIXED IS DELTA 0.01 RANGE -1.0 .. 1.0;

     GENERIC
          FL : IN OUT FLOATING DIGITS 2;   -- ERROR: CONSTRAINT.
          FI : FIXED DELTA 0.1;            -- ERROR: CONSTRAINT.
     PACKAGE PKG IS
     END PKG;

     GENERIC
          FL : FLOATING DIGITS 2;          -- ERROR: CONSTRAINT.
          FI : IN OUT FIXED DELTA 0.1;     -- ERROR: CONSTRAINT.
     FUNCTION CHECK ( X : INTEGER ) RETURN BOOLEAN;

     FUNCTION CHECK ( X : INTEGER ) RETURN BOOLEAN IS
     BEGIN
          RETURN FALSE;
     END CHECK;

BEGIN     -- BC1109B
     NULL;
END BC1109B;
