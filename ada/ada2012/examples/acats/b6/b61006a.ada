-- B61006A.ADA

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
-- CHECK THAT THE TYPE OF A DEFAULT EXPRESSION MUST BE THE SAME AS THE
-- BASE TYPE OF THE FORMAL PARAMETER.

-- BHS 6/29/84

PROCEDURE B61006A IS

     SUBTYPE STYPE IS INTEGER;
     TYPE DER_INT IS NEW INTEGER;
     TYPE ARR IS ARRAY (1..3) OF INTEGER;
     TYPE DER_ARR IS ARRAY (1..3) OF DER_INT;

     I_VAR   : INTEGER := 4;
     S_VAR   : STYPE := STYPE(I_VAR);
     DER_VAR : DER_INT := DER_INT(5);
     AR1     : ARR := (1,2,3);
     AR2     : ARRAY (1..3) OF INTEGER := (1,2,3);

     FUNCTION F (I : INTEGER) RETURN DER_INT IS
     BEGIN
          RETURN DER_INT(I);
     END F;


     PROCEDURE P1 (X : INTEGER := DER_VAR) IS     -- ERROR: INTEGER WITH
                                                  -- DERIVED TYPE
     BEGIN
          NULL;
     END P1;


     PROCEDURE P2 (X : DER_INT := I_VAR + INTEGER(DER_VAR)) IS -- ERROR:
                                                        -- DERIVED TYPE
                                                        -- WITH INTEGER
     BEGIN
          NULL;
     END P2;


     PROCEDURE P3 (X : ARR := AR2) IS           -- ERROR: ARRAY TYPE
                                                -- WITH ANONYMOUS ARRAY
     BEGIN
          NULL;
     END P3;


     PROCEDURE P4 (X : DER_INT := AR1(1)) IS     -- ERROR: DERIVED TYPE
                                                 -- WITH INTEGER
     BEGIN
          NULL;
     END P4;


     PROCEDURE P5 (X : DER_INT := AR2'FIRST) IS  -- ERROR: DERIVED TYPE
                                                 -- WITH INTEGER
     BEGIN
          NULL;
     END P5;


     PROCEDURE P6 (X : DER_ARR := AR1) IS    -- ERROR: ARRAY OF DERIVED
                                             -- WITH ARRAY OF INTEGER
     BEGIN
          NULL;
     END P6;


     PROCEDURE P7 (X : INTEGER := F(I_VAR)) IS   -- ERROR: INTEGER
                                                 -- WITH DERIVED
     BEGIN
          NULL;
     END P7;


     PROCEDURE P8 (X : STYPE := I_VAR) IS    -- OK; SAME BASE TYPE
     BEGIN
          NULL;
     END P8;


     PROCEDURE P9 (X : INTEGER := S_VAR) IS   -- OK
     BEGIN
          NULL;
     END P9;


     PROCEDURE P10 (X : DER_INT := AR2'LENGTH) IS   -- OK; IMPLICIT
                                                    -- CONVERSION
     BEGIN
          NULL;
     END P10;


BEGIN
     NULL;

END B61006A;
