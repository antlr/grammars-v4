-- B83F02B.ADA

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
-- CHECK THAT A LABEL DEFINED IN A PACKAGE BODY CANNOT BE IDENTICAL
--    TO AN IDENTIFIER DECLARED IN THE CORRESPONDING
--    PACKAGE SPECIFICATION OR BODY.


--    RM    18 AUGUST 1980


PROCEDURE  B83F02B  IS

     PACKAGE  P  IS

          X1 : BOOLEAN := FALSE ;
          X2 : INTEGER RANGE 1..23 := 11 ;
          TYPE  T2  IS  ( U , V , W );
          X3      : T2 ;
          SUBTYPE  T3  IS  T2 RANGE U..V ;
          SUBTYPE  T5  IS INTEGER RANGE 3..7 ;
          
          PACKAGE  P2  IS
               I : INTEGER ;
          END P2 ;

          PROCEDURE  PROC3 ;

     END  P ;


     PACKAGE BODY  P  IS

          YY1 : INTEGER ;
          YY3 : BOOLEAN := TRUE ;
          TYPE  TT1  IS  ( A , B , C) ;
          X   : T2 ;
          SUBTYPE  TT2  IS  INTEGER RANGE 1..2 ;

          PROCEDURE  PROC3  IS
          BEGIN
               NULL ;
          END PROC3 ;

     BEGIN

          -- 'ERROR 1'  -  ALREADY DECLARED IN PACKAGE SPECIFICATION
          -- 'ERROR 2'  -  ALREADY DECLARED IN PACKAGE BODY

          << X1 >>      -- ERROR: 1     BOOLEAN VARIABLE
          << X2 >>      -- ERROR: 1     INTEGER VARIABLE
          << X3 >>      -- ERROR: 1     ENUMERATION VARIABLE
          << T2 >>      -- ERROR: 1     ENUMERATION TYPE
          << T3 >>      -- ERROR: 1     ENUMERATION SUBTYPE
          << T5 >>      -- ERROR: 1     INTEGER SUBTYPE
          << P2 >>      -- ERROR: 1     PACKAGE
          << PROC3 >>   -- ERROR: 1     PROCEDURE

               NULL ;

          << X  >>      -- ERROR: 2     ENUMERATION VARIABLE
          << YY1 >>     -- ERROR: 2     INTEGER VARIABLE
          << YY2 >>
          << YY3 >>     -- ERROR: 2     BOOLEAN VARIABLE
          << TT1 >>     -- ERROR: 2     TYPE
          << TT2 >>     -- ERROR: 2     SUBTYPE
          << TT3 >>

               NULL ;

     END P ;


BEGIN

     NULL ;

END B83F02B;
