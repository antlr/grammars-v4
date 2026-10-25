-- B54B02B.ADA

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
-- CHECK THAT IF A CASE EXPRESSION IS A VARIABLE, CONSTANT,
--    TYPE CONVERSION, ATTRIBUTE (IN PARTICULAR 'FIRST AND 'LAST),
--    FUNCTION INVOCATION, QUALIFIED EXPRESSION, OR A PARENTHESIZED
--    EXPRESSION HAVING ONE OF THESE FORMS, AND THE SUBTYPE OF THE
--    EXPRESSION IS NON-STATIC, AN  'OTHERS'  MUST NOT BE OMITTED IF ONE
--    OR MORE OF THE VALUES IN THE BASE TYPE'S RANGE ARE MISSING.


--     RM  01/27/80
--     SPS 11/22/82
--     SPS 02/02/83
--     RLB 02/08/18 ADDED ERROR LOCATION INDICATORS TO REFLECT COMMON
--                  ERROR REPORTING STRATEGIES.

PROCEDURE  B54B02B  IS
BEGIN

     -- THE TEST CASES APPEAR IN THE FOLLOWING ORDER:
     --
     --         (A)    VARIABLES (INTEGER , BOOLEAN)
     --         (B)    CONSTANTS
     --         (C)    ATTRIBUTES ('FIRST, 'LAST)
     --         (D)    FUNCTION CALLS
     --         (E)    QUALIFIED EXPRESSIONS
     --         (F)    TYPE CONVERSIONS
     --         (G)    PARENTHESIZED EXPRESSIONS OF THE ABOVE KINDS


     DECLARE   --  NON-STATIC RANGES

          INT_5  : INTEGER   :=  5  ;
          CHAR_Q : CHARACTER := 'Q' ;
          SUBTYPE  STAT   IS  INTEGER RANGE 1..50 ;
          SUBTYPE  DYN    IS  STAT    RANGE 1..INT_5 ;
          I   : STAT RANGE 1..INT_5 ;
          J   : DYN ;
          SUBTYPE  DYNCHAR  IS
                CHARACTER  RANGE  ASCII.NUL .. CHAR_Q ;
          SUBTYPE  STATCHAR  IS
                DYNCHAR RANGE 'A' .. 'C' ;
          CHAR: DYNCHAR ;
          TYPE  ENUMERATION  IS  ( A,B,C,D,E,F,G,H,K,L,M,N );
          SUBTYPE  STATENUM  IS
                ENUMERATION RANGE  A .. L ;
          SUBTYPE  DYNENUM  IS
                STATENUM  RANGE  A .. ENUMERATION'VAL(INT_5);
          ENUM: DYNENUM ;
          CONS: CONSTANT DYNCHAR  :=  'A';

          FUNCTION  FF  RETURN DYN  IS
          BEGIN
               RETURN  2 ;
          END  FF ;

     BEGIN

          I  :=  2 ;
          J  :=  2 ;

          CASE  I  IS
               WHEN  3 | 5  =>  NULL ;
               WHEN  2 | 4  =>  NULL ;
               WHEN  INTEGER'FIRST..0 | 6..INTEGER'LAST  =>  NULL ;
          END CASE;  -- ERROR: {4:11;1} MISSING 'OTHERS' A

          CASE  I  IS
               WHEN  3 | 5      =>  NULL ;
               WHEN  2 | 4      =>  NULL ;
               WHEN  1 | 6..50  =>  NULL ;
          END CASE;  -- ERROR: {4:11;1} MISSING 'OTHERS' A

          CASE  J  IS
               WHEN  1 | 3 | 5  =>  NULL ;
               WHEN  2 | 4      =>  NULL ;
               WHEN  INTEGER'FIRST..0 | 7..INTEGER'LAST  =>  NULL ;
          END CASE;  -- ERROR: {4:11;1} MISSING 'OTHERS' A

          CASE  J  IS
               WHEN  1 | 3 | 5  =>  NULL ;
               WHEN  2 | 4      =>  NULL ;
          END CASE;  -- ERROR: {3:11;1} MISSING 'OTHERS' A

          CASE  CONS  IS
                WHEN CHARACTER'FIRST..'A'  =>  NULL;
                WHEN 'C'..CHARACTER'LAST  =>  NULL;
          END CASE;  -- ERROR: {3:11;1} MISSING 'OTHERS' B

          CASE  DYN'FIRST  IS
               WHEN  1..STAT'LAST  =>  NULL;
          END CASE;  -- ERROR: {2:11;1} MISSING 'OTHERS' C

          CASE  STATCHAR'LAST  IS
               WHEN  'A'..'C'  =>  NULL;
          END CASE;  -- ERROR: {2:11;1} MISSING 'OTHERS' C

          CASE  FF  IS
               WHEN  4..5  =>  NULL ;
               WHEN  INTEGER'FIRST..-1 | 6..INTEGER'LAST  =>  NULL ;
               WHEN  1..3  =>  NULL ;
          END CASE;  -- ERROR: {4:11;1} MISSING 'OTHERS' D

          CASE  DYN'( 2 )  IS
               WHEN  INTEGER'FIRST..-1 | 6..INTEGER'LAST  =>  NULL ;
               WHEN  5 | 1..4  =>  NULL ;
          END CASE;  -- ERROR: {3:11;1} MISSING 'OTHERS' E

          CASE  DYN( J )  IS
               WHEN  5 | 2..4  =>  NULL ;
               WHEN  1         =>  NULL ;
               WHEN  6..50     =>  NULL ;
          END CASE;  -- ERROR: {4:11;1} MISSING 'OTHERS' F


          CASE  ( CHAR )  IS
               WHEN  ASCII.NUL .. 'Q'  =>  NULL ;
               WHEN  'R' .. 'Y'        =>  NULL ;
          END CASE;  -- ERROR: {3:11;1} MISSING 'OTHERS'  G A

          CASE  ( ENUM )  IS
               WHEN  A | C | E  =>  NULL ;
               WHEN  B | D      =>  NULL ;
               WHEN  F .. M     =>  NULL ;
          END CASE;  -- ERROR: {4:11;1} MISSING 'OTHERS'  G A

          CASE  ( FF )  IS
               WHEN  1 | 5  =>  NULL ;
               WHEN  2 | 4  =>  NULL ;
               WHEN  3      =>  NULL ;
          END CASE;  -- ERROR: {4:11;1} MISSING 'OTHERS'  G D

          CASE  ( DYN'( I ) )  IS
               WHEN  4..5  =>  NULL ;
               WHEN  1..3  =>  NULL ;
               WHEN  INTEGER'FIRST..0 | 6..50         =>  NULL ;
          END CASE;  -- ERROR: {4:11;1} MISSING 'OTHERS'  G E

          CASE  ( DYN( 2 ) )  IS
               WHEN  2..4  =>  NULL ;
               WHEN  1     =>  NULL ;
               WHEN  5..50 =>  NULL ;
          END CASE;  -- ERROR: {4:11;1} MISSING 'OTHERS'  G F

          CASE  (CONS)  IS
               WHEN ASCII.NUL..'Q'  =>  NULL;
          END CASE;  -- ERROR: {2:11;1} MISSING 'OTHERS'  G B

          CASE  (DYNENUM'LAST)  IS
              WHEN A..M  =>  NULL;
          END CASE;  -- ERROR: {2:11;1} MISSING 'OTHERS'  G C

     END ;


END B54B02B ;
