-- B35506D.ADA

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
-- CHECK THAT THE ARGUMENT TO T'VAL MUST BE AN INTEGER TYPE.

-- RJW 2/26/86

PROCEDURE  B35506D  IS
     
     I1 : INTEGER := 0;

     TYPE INT1 IS RANGE 0 .. 2;
     I2 : INT1 := 0;

     TYPE INT2 IS NEW INTEGER;
     I3 : INT2 := 0;

     F : FLOAT := 0.0;
     
     TYPE FIX IS DELTA 1.0 RANGE 0.0 .. 2.0;
     FX : FIX := 0.0;

     R : CONSTANT := 0.0;
     
     TYPE T IS (A, B, C);
     T1 : T;

BEGIN
     T1 := T'VAL (I1);                   -- OK.
     T1 := T'VAL (I2);                   -- OK.
     T1 := T'VAL (I3);                   -- OK.
     T1 := T'VAL (0);                    -- OK.
     T1 := T'VAL (F);                    -- ERROR: F.
     T1 := T'VAL (FX);                   -- ERROR: FX.
     T1 := T'VAL (R);                    -- ERROR: R.
     T1 := T'VAL (A);                    -- ERROR: A.
     T1 := T'VAL (TRUE);                 -- ERROR: TRUE.
     T1 := T'VAL ('0');                  -- ERROR: '0'.
END B35506D ;
