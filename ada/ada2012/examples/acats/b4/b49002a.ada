-- B49002A.ADA

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
-- CHECK THAT NO EXPRESSION HAVING A SCALAR GENERIC FORMAL TYPE (OR TYPE
-- DERIVED INDIRECTLY FROM A GENERIC FORMAL TYPE) IS CONSIDERED STATIC.

-- L.BROWN  08/20/86

PROCEDURE B49002A IS

     GENERIC
          TYPE T IS RANGE <> ;
     PACKAGE PACK IS
          TYPE ARR IS ARRAY(T RANGE 0 .. 1) OF BOOLEAN;
          X : ARR := (0 => FALSE, T'(1) => TRUE);              -- ERROR:
     END PACK;

     GENERIC
          TYPE T IS RANGE <> ;
     FUNCTION FUN RETURN INTEGER ;
     FUNCTION FUN RETURN INTEGER IS
          TYPE S IS NEW T ;
          TYPE ARR IS ARRAY(S RANGE 0 .. 1) OF INTEGER;
          X : ARR := (S'(0) => 4, 1 => 5);                     -- ERROR:
          XR : INTEGER RANGE 1 .. 10 := 5;
     BEGIN
          RETURN XR;
     END FUN;

     GENERIC
          TYPE T IS RANGE <> ;
     PROCEDURE PROC ;
     PROCEDURE PROC IS
          TYPE SNEW IS NEW T ;
          TYPE SNEW2 IS NEW SNEW ;
          TYPE ARR IS ARRAY(SNEW2 RANGE 0 .. 1) OF INTEGER;
          X : ARR := (0 => 2, SNEW2'(1) => 3);                 -- ERROR:
     BEGIN
          NULL;
     END PROC;

     PACKAGE PACK1 IS NEW PACK(INTEGER);
     FUNCTION FUN2 IS NEW FUN(INTEGER);
     PROCEDURE PROC2 IS NEW PROC(INTEGER);

BEGIN
     NULL;
END B49002A;
