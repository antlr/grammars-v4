-- B41101A.ADA

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
-- CHECK THAT NEITHER TOO FEW NOR TOO MANY INDEX VALUES
--   ARE ACCEPTED IN INDEXED_COMPONENTS.

-- WKB 8/11/81
-- SPS 12/10/82

PROCEDURE B41101A IS

     TYPE T1 IS ARRAY (1..5) OF INTEGER;
     TYPE T2 IS ARRAY (1..5, 1..5) OF INTEGER;
     TYPE T3 IS ACCESS T1;
     TYPE T4 IS ACCESS T2;

     A : T1 := (1,2,3,4,5);
     B : T2 := (1..5 => (1..5 => 2));
     C : T3 := NEW T1 '(1,2,3,4,5);
     D : T4 := NEW T2 '(1..5 => (1..5 => 4));
     I : INTEGER;

BEGIN

     I := A(1,2);             -- ERROR: TOO MANY INDICES.
     NULL;
     I := C(3,1);             -- ERROR: TOO MANY INDICES.
     NULL;
     I := B(1,2,3);           -- ERROR: TOO MANY INDICES.
     NULL;
     I := B(4);               -- ERROR: TOO FEW INDICES.
     NULL;
     I := D(2);               -- ERROR: TOO FEW INDICES.

END B41101A;
