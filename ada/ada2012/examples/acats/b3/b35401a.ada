-- B35401A.ADA

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
-- CHECK THAT THE BOUNDS IN AN INTEGER TYPE DEFINITION MUST BE STATIC.

-- RJW 2/20/86
-- PWN 11/05/95  REMOVED CHCKS FOR STATIC EXPRESSIONS WHERE RULES RELAXED.
-- PWN 03/21/96  Restored checks in Ada 95 legal format.


PROCEDURE B35401A IS

     I1 : INTEGER := 1;
     I2 : INTEGER := 2000;

     ICON : CONSTANT INTEGER := I1;
 
     FUNCTION F (I : INTEGER) RETURN INTEGER;

     TYPE PAGE_NUM1 IS RANGE I1 .. 2000;        -- ERROR: NOT STATIC.
     TYPE PAGE_NUM2 IS RANGE 1 .. I2;           -- ERROR: NOT STATIC.
     TYPE OK IS ARRAY(1..5) OF INTEGER;
     TYPE LINE_SIZE IS RANGE OK'FIRST .. 10;    -- OK.
     TYPE LENGTH_SIZE IS RANGE 1 .. OK'LENGTH;  -- OK.
     TYPE SMALL_INT1 IS RANGE ICON .. 10;       -- ERROR: NOT STATIC.
     TYPE SMALL_INT2 IS RANGE 1 .. ICON;        -- ERROR: NOT STATIC.
     TYPE BUFFER_SIZE IS RANGE F(2) .. 10;      -- ERROR: NOT STATIC.
     TYPE HIS_INT IS RANGE 1 .. F(5);           -- ERROR: NOT STATIC.

     FUNCTION F (I : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN I;
     END F;
BEGIN
     NULL;
END B35401A;
