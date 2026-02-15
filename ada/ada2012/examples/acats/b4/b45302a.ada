-- B45302A.ADA

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
-- CHECK THAT '&' IS NOT PREDEFINED FOR MULTIDIMENSIONAL ARRAYS HAVING
-- THE SAME TYPE OR FOR ONE DIMENSIONAL ARRAYS RELATED BY DERIVATION
-- (INCLUDING DERIVED INDEX TYPES).

-- RJW 2/8/86
-- PWN 12/19/94  CORRECTED -- ERROR: INCONSITENCIES

PROCEDURE B45302A IS
     
BEGIN

     DECLARE
          TYPE ARR IS ARRAY( 1 .. 2, 1 .. 2 ) OF CHARACTER;

          A : ARR := ( OTHERS => ( OTHERS => 'A' ));
          B : ARR := ( OTHERS => ( OTHERS => 'A' ));
     BEGIN
          A := A & B;               -- ERROR: MULTI-DIM TYPES FOR '&'.
     END;

     DECLARE
          TYPE ARR1 IS ARRAY ( 1 .. 1 ) OF CHARACTER;
          TYPE ARR2 IS NEW ARR1;
          TYPE INT  IS  NEW INTEGER RANGE 1 .. 1;          
          TYPE ARR3 IS ARRAY ( INT ) OF CHARACTER;
          TYPE CHAR IS NEW CHARACTER;
          TYPE ARR4 IS ARRAY ( 1 .. 1 ) OF CHAR;
          TYPE ARR5 IS ARRAY ( 1 .. 1 ) OF CHARACTER;
          
          A1 : ARR1 := "A"; 
          A2 : ARR2 := "A";  
          A3 : ARR3 := "A";  
          A4 : ARR4 := "A";  
          A5 : ARR5 := "A";  
     BEGIN
          A1 := A1 & A2;            -- ERROR: DIFFERENT TYPES FOR '&'.
          A1 := A1 & A3;            -- ERROR: DIFFERENT TYPES FOR '&'.
          A1 := A1 & A4;            -- ERROR: DIFFERENT TYPES FOR '&'.
          A1 := A1 & A5;            -- ERROR: DIFFERENT TYPES FOR '&'.
     END;

END B45302A;
