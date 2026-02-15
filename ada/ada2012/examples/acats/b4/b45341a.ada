-- B45341A.ADA

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
-- CHECK THAT '&' IS NOT PREDEFINED FOR MULTIDIMENSIONAL ARRAYS 
-- OR FOR ARRAYS HAVING A LIMITED COMPONENT TYPE.

-- RJW 2/10/86

PROCEDURE B45341A IS
     
BEGIN

     DECLARE
          TYPE ARR IS ARRAY( 1 .. 2, 1 .. 2 ) OF CHARACTER;

          A : ARR := (OTHERS => (OTHERS => 'A'));
          B : ARR := (OTHERS => (OTHERS => 'B'));
     BEGIN
          A := A & B;               -- ERROR: INVALID TYPES FOR '&'.
     END;

     DECLARE
          PACKAGE PKG IS
               TYPE ARR1 IS LIMITED PRIVATE;
               TYPE CHAR IS LIMITED PRIVATE;
               TYPE ARR2 IS ARRAY(1 .. 2) OF CHAR;
          PRIVATE
               TYPE CHAR IS NEW CHARACTER;
               TYPE ARR1 IS ARRAY(1 .. 2) OF CHARACTER;
          END PKG;

          USE PKG;

          A1, B1 : ARR1;
          A2, B2 : ARR2;

     BEGIN
          A1 := A1 & B1;            -- ERROR: INVALID TYPES FOR '&'.
          A2 := A2 & B2;            -- ERROR: INVALID TYPES FOR '&'.
     END;

END B45341A;
