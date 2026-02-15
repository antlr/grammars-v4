-- B48003E.ADA

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
-- CHECK THAT ILLEGAL FORMS OF ALLOCATORS ARE FORBIDDEN. IN PARTICULAR,
-- FOR ALLOCATORS OF THE FORM "NEW T'(X)", CHECK THAT IF T IS AN
-- ARRAY TYPE, CHECK ALL FORMS OF ILLEGAL ARRAY AGGREGATE.

-- EG  08/27/84
-- WMC 04/07/92 REMOVED UNNECESSARY REFERENCES TO PACKAGE REPORT AND
--              PROCEDURE IDENT_INT.
-- RLB 11/19/19  Added error location indicators.

PROCEDURE B48003E IS

     TYPE T IS (A, B, C, D);
     TYPE CA IS ARRAY(1 .. 4) OF INTEGER;
     TYPE A_CA IS ACCESS CA;

     V1, V2, V3, V4, V5, V6, V7, V8 : A_CA;

     FUNCTION IDENT ( X : IN INTEGER ) RETURN INTEGER IS
        BEGIN
           RETURN X;
        END IDENT;

BEGIN

     V1 := NEW CA'(1, 2, 3 => 3, 4 => 4);                   -- ERROR:  {12}
     V2 := NEW CA'(4 .. 2 => 3, 1 => 2);                    -- ERROR:  {12}
     V3 := NEW CA'(IDENT(1) .. IDENT(3) => 2, OTHERS => 1); -- ERROR:  {12}
     V4 := NEW CA'(2 => 1, 4 => 2, 1 => 3);                 -- ERROR:  {12}
     V5 := NEW CA'(2 => 1, 4 => 2, 1 => 3, 2 => 1);         -- ERROR:  {12}
     V6 := NEW CA'(2 => 1, A => 2, 3 => 4, 4 => 3);         -- ERROR:  {12}
     V7 := NEW CA'(1 .. 4 => B);                            -- ERROR:  {12}
     V8 := NEW CA'(IDENT(1) .. IDENT(2) => 2,
                   IDENT(3) .. IDENT(4) => 3);              -- ERROR:  {1:12}

END B48003E;
