-- B35709A.ADA

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
-- DISTINCTION OF REAL TYPES: THREE SUBTESTS
-- (A) TWO TEXTUALLY IDENTICAL TYPES ARE DISTINCT
-- (B) TWO TYPES DERIVED FROM FLOAT ARE DISTINCT
-- (C) SUBTYPES OF DISTINCT TYPES ARE DISTINCT

-- BAW 5 SEPT 80

PROCEDURE B35709A IS
     TYPE D1 IS DIGITS 5;
     TYPE D2 IS DIGITS 5;
     XD1: D1 := 1.0;
     XD2: D2 := XD1;       -- ERROR: WRONG TYPES (A)

     TYPE D3 IS NEW FLOAT;
     TYPE D4 IS NEW FLOAT;
     XD3: D3 := 1.0;
     XD4: D4 := XD3;       -- ERROR: WRONG TYPES (B)

     SUBTYPE D5 IS D1 RANGE 0.0 .. 2.0;
     SUBTYPE D6 IS D2 RANGE 0.0 .. 2.0;
     XD5: D5 := 1.0;
     XD6: D6 := XD5;       -- ERROR: WRONG TYPES (C)
BEGIN
     NULL;
END B35709A;
