-- B54B01B.TST

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
-- CHECK THAT IF A CASE EXPRESSION IS A CONSTANT, VARIABLE,
--    TYPE CONVERSION, OR QUALIFIED EXPRESSION AND THE SUBTYPE OF THE
--    EXPRESSION IS STATIC, AN  'OTHERS'  MUST NOT BE OMITTED IF ONE
--    OR MORE OF THE VALUES IN THE SUBTYPE'S RANGE ARE MISSING.


--     RM  01/27/80
--     SPS 12/10/82
--     SPS 02/09/83
--     JBG 06/09/83
--     RLB 02/08/18 ADDED ERROR LOCATION INDICATORS TO REFLECT COMMON
--                  ERROR REPORTING STRATEGIES.

WITH SYSTEM;
PROCEDURE  B54B01B  IS
BEGIN

     -- THE TEST CASES APPEAR IN THE FOLLOWING ORDER:
     --
     --    I.   CONSTANTS
     --
     --    II.  STATIC SUBRANGES
     --
     --         (A)    VARIABLES (INTEGER , BOOLEAN)
     --         (B)    QUALIFIED EXPRESSIONS
     --         (C)    TYPE CONVERSIONS

     DECLARE  -- CONSTANTS

          T    :  CONSTANT BOOLEAN  :=  TRUE ;
          FOUR :  CONSTANT  :=  4 ;

     BEGIN


          CASE T  IS
               WHEN  FALSE             =>  NULL ;
          END CASE;  -- ERROR: {2:11;1} MISSING 'OTHERS'

          CASE  FOUR  IS
               WHEN  $INTEGER_FIRST .. 1  =>  NULL ;
               WHEN  2 ..  $INTEGER_LAST  =>  NULL ;
          END CASE;  -- ERROR: {3:11;1} MISSING 'OTHERS'

          CASE  FOUR  IS
               WHEN SYSTEM.MIN_INT .. SYSTEM.MAX_INT => NULL;
          END CASE;  -- ERROR: {2:11;1} MISSING 'OTHERS'.

     END ;


     DECLARE   --  STATIC SUBRANGES

          SUBTYPE  STAT  IS  INTEGER RANGE 1..5 ;
          I   : INTEGER RANGE 1..5 := 1;
          J   : STAT := 1;
          BOOL: BOOLEAN := FALSE;
          CHAR: CHARACTER := 'A';
          TYPE  ENUMERATION  IS  ( FIRST,SECOND,THIRD,FOURTH,FIFTH );
          ENUM: ENUMERATION := FIRST;

    BEGIN

          I  :=  2 ;
          J  :=  2 ;

          CASE  I  IS
               WHEN  1 | 3  =>  NULL ;
               WHEN  2 | 4  =>  NULL ;
          END CASE;  -- ERROR: {3:11;1} MISSING 'OTHERS' II A

          CASE  BOOL  IS
               WHEN  TRUE   =>  NULL ;
          END CASE;  -- ERROR: {2:11;1} MISSING 'OTHERS' II A

          CASE  STAT'( 2 )  IS
               WHEN  5 | 2..4  =>  NULL ;
          END CASE;  -- ERROR: {2:11;1} MISSING 'OTHERS' II B

          CASE  STAT( J )  IS
               WHEN  5 | 2..3  =>  NULL ;
               WHEN  1         =>  NULL ;
          END CASE;  -- ERROR: {3:11;1} MISSING 'OTHERS' II C

     END ;     --  STATIC SUBRANGES

END B54B01B ;
