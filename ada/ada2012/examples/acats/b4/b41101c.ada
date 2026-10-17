-- B41101C.ADA

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
-- CHECK THAT THE BASE TYPE OF THE SUBSCRIPT OF AN INDEXED
--   COMPONENT MUST BE THE SAME AS THE BASE TYPE OF THE INDEX
--   OF THE ARRAY TYPE.

-- WKB 8/12/81
-- JBG 8/21/83

PROCEDURE B41101C IS

     SUBTYPE S IS INTEGER RANGE 5..50;
     TYPE S1 IS NEW S;
     TYPE S2 IS ARRAY (S1 RANGE 5..10) OF INTEGER;
     X : S1 := 6;
     Y : S2 := (5..10 => 1);

     TYPE T IS ARRAY (INTEGER RANGE 0..20) OF INTEGER;
     A : T := (0..20 => 2);
     I : INTEGER;
     J : INTEGER := 6;

     TYPE U IS (E1,E2,E3,E4);
     U1 : ARRAY (U) OF INTEGER;

BEGIN

     I := A(X);               -- ERROR: DIFFERENT INDEX BASE TYPES.
     NULL;
     I := A(E2);              -- ERROR: DIFFERENT INDEX BASE TYPES.
     NULL;
     I := A(FALSE);           -- ERROR: DIFFERENT INDEX BASE TYPES.
     NULL;
     I := A(ASCII.CR);        -- ERROR: DIFFERENT INDEX BASE TYPES.
     NULL;
     I := Y(J);               -- ERROR: DIFFERENT INDEX BASE TYPES.
     NULL;
     I := U1(3);              -- ERROR: DIFFERENT INDEX BASE TYPES.

END B41101C;
