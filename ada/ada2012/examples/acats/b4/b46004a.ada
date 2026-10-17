-- B46004A.ADA

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
-- WHEN THE TARGET TYPE OF A TYPE CONVERSION IS AN ARRAY TYPE, CHECK 
-- THAT THE OPERAND TYPE CANNOT BE AN ENUMERATION TYPE, A RECORD TYPE,
-- AN ACCESS TYPE, OR A PRIVATE TYPE WHOSE FULL DECLARATION DECLARES 
-- AN ARRAY TYPE.

-- R.WILLIAMS 9/19/86

PROCEDURE B46004A IS

     TYPE ENUM IS (E1, E2, E3, E4);

     TYPE ARR IS ARRAY (1 .. 1) OF ENUM;
     A1 : ARR;

     TYPE REC IS
          RECORD 
               A : ARR;
          END RECORD;
     R1 : REC := (A => (1 => E1));
     
     TYPE ARR_NAME IS ACCESS ARR;
     AN : ARR_NAME := NEW ARR'(1 => E1);

     PACKAGE PKG IS 
          TYPE PRIV1 IS PRIVATE;
          P1 : CONSTANT PRIV1;

          TYPE PRIV2 IS PRIVATE;
          P2 : CONSTANT PRIV2;

     PRIVATE
          TYPE PRIV1 IS NEW ARR;
          P1 : CONSTANT PRIV1 := (1 => E1);

          TYPE PRIV2 IS ARRAY (1 .. 1) OF ENUM;
          P2 : CONSTANT PRIV2 := (1 => E1);
     END PKG;

     USE PKG;
     
BEGIN
     A1 := ARR (E1);               -- ERROR: OPERAND IS ENUM TYPE.

     A1 := ARR (R1);               -- ERROR: OPERAND IS RECORD TYPE.
     
     A1 := ARR (AN);               -- ERROR: OPERAND IS ACCESS TYPE.

     A1 := ARR (P1);               -- ERROR: OPERAND IS PRIVATE TYPE.

     A1 := ARR (P2);               -- ERROR: OPERAND IS PRIVATE TYPE.
END B46004A;
