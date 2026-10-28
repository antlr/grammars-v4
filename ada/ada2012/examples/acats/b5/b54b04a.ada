-- B54B04A.ADA

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
-- CHECK THAT EVEN WHEN THE CONTEXT INDICATES THAT A CASE EXPRESSION
--    COVERS A SMALLER RANGE OF VALUES THAN PERMITTED BY ITS SUBTYPE,
--    AN  'OTHERS'  ALTERNATIVE IS REQUIRED IF THE SUBTYPE VALUE RANGE
--    (ASSUMED TO BE STATIC) IS NOT FULLY COVERED.

-- PART  I :  STATIC SUBRANGES OF STATIC RANGES


--     RM  01/29/80
--     SPS 02/02/83
--     RLB 02/08/18 ADDED ERROR LOCATION INDICATORS TO REFLECT COMMON
--                  ERROR REPORTING STRATEGIES.


PROCEDURE  B54B04A  IS
BEGIN

     -- THE TEST CASES APPEAR IN THE FOLLOWING ORDER:
     --
     --    I.  STATIC SUBRANGES OF STATIC RANGES
     --
     --         (A)    VARIABLES (INTEGER , BOOLEAN)
     --         (C)    QUALIFIED EXPRESSIONS
     --         (D)    CONVERSIONS
     --         (E)    PARENTHESIZED EXPRESSIONS OF THE ABOVE KINDS



     DECLARE   --  STATIC SUBRANGES OF STATIC RANGES

          SUBTYPE  STAT  IS  INTEGER RANGE 1..5 ;
          TYPE  ENUMERATION  IS  ( FIRST,SECOND,THIRD,FOURTH,FIFTH );
          I   : INTEGER RANGE 1..5   := 2 ;
          J   : STAT                 := 2 ;
          BOOL: BOOLEAN              := TRUE  ;
          CHAR: CHARACTER            := 'U' ;
          ENUM: ENUMERATION          := THIRD ;

     BEGIN

          CASE  I  IS

               WHEN  2  =>
                    CASE  I  IS
                         WHEN  1 | 3  =>  NULL ;
                         WHEN  2 | 4  =>  NULL ;
                    END CASE;  -- ERROR: {3:21;1} MISSING 'OTHERS' I A

               WHEN OTHERS  =>
                    NULL ;

          END CASE;


          IF  BOOL  THEN

               CASE  BOOL  IS
                    WHEN  TRUE   =>  NULL ;
               END CASE;  -- ERROR: {2:16;1} MISSING 'OTHERS' I A

          END IF;


          CASE  STAT'( 2 )  IS

               WHEN  2  =>
                    CASE  STAT'( 2 )  IS
                         WHEN  5 | 2..4  =>  NULL ;
                    END CASE;  -- ERROR: {2:21;1} MISSING 'OTHERS' I C

               WHEN OTHERS  =>
                    NULL ;

          END CASE;


          CASE  STAT( J )  IS

               WHEN  2  =>
                    CASE  STAT( J )  IS
                         WHEN  5 | 2..3  =>  NULL ;
                         WHEN  1         =>  NULL ;
                    END CASE;  -- ERROR: {3:21;1} MISSING 'OTHERS' I D

               WHEN OTHERS  =>
                    NULL ;

          END CASE;


     END ;     --  STATIC SUBRANGES OF STATIC RANGES



END B54B04A ;
