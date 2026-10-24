-- B54B04B.ADA

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
--    AN  'OTHERS'  ALTERNATIVE IS REQUIRED IF THE CASE EXPRESSION HAS A
--    NONSTATIC SUBTYPE AND THE RANGE OF THE BASE TYPE IS NOT COVERED.

-- PART  II :  STATIC SUBRANGES OF NONSTATIC SUBTYPES


--     RM  01/29/80
--     SPS 02/02/83
--     JBG 04/19/83
--     RLB 02/08/18 ADDED ERROR LOCATION INDICATORS TO REFLECT COMMON
--                  ERROR REPORTING STRATEGIES.

PROCEDURE  B54B04B  IS
BEGIN

     -- THE TEST CASES APPEAR IN THE FOLLOWING ORDER:
     --
     --    I.
     --    II.  STATIC SUBRANGES OF NONSTATIC SUBTYPES
     --
     --         (A)    VARIABLES (INTEGER , BOOLEAN)
     --         (B)    QUALIFIED EXPRESSIONS
     --         (C)    CONVERSIONS
     --         (D)    PARENTHESIZED EXPRESSIONS OF THE ABOVE KINDS



     DECLARE   --  STATIC SUBRANGES OF NONSTATIC SUBTYPES

          INT_50   :  INTEGER    :=  50 ;
          INT_11   :  INTEGER    :=  11 ;
          CHAR_Z   :  CHARACTER  :=  'Z';
          SUBTYPE  DYNAM  IS  INTEGER RANGE 1..INT_50;
          SUBTYPE  NONSTAT   IS  DYNAM   RANGE 1..5;
          SUBTYPE  DYNCHAR  IS
                CHARACTER RANGE  ASCII.NUL .. CHAR_Z;
          SUBTYPE  NONSTATCHAR  IS
                DYNCHAR RANGE  ASCII.NUL .. 'Y';
          TYPE  ENUMERATION  IS  ( A,B,C,D,E,F,G,H,K,L,M,N );
          SUBTYPE  DYNENUM  IS
                ENUMERATION  RANGE  A .. ENUMERATION'VAL(INT_11);
          SUBTYPE  NONSTATENUM  IS
                DYNENUM RANGE  A .. E;
          I   : DYNAM RANGE 1..5     :=  2 ;
          J   : NONSTAT                 :=  2 ;
          CHAR: NONSTATCHAR             := 'U';
          ENUM: NONSTATENUM             :=  D ;

     BEGIN

          CASE  I  IS

               WHEN  2  =>
                    CASE  I  IS
                         WHEN  1 | 3   =>  NULL;
                         WHEN  2 | 4   =>  NULL;
                    END CASE;  -- ERROR: {3:21;1} MISSING 'OTHERS' II A

               WHEN OTHERS  =>
                    NULL;

          END CASE;


          J := 2;
          CASE  J  IS
               WHEN  1 .. 3   =>  NULL;
               WHEN  5        =>  NULL;
          END CASE;  -- ERROR: {3:11;1} MISSING 'OTHERS' II A


          J := 2;
          IF  J = 2  THEN
              CASE  J  IS
                   WHEN  1 .. 3   =>  NULL;
                   WHEN  5    =>  NULL;
              END CASE;  -- ERROR: {3:15;1} MISSING 'OTHERS' II A
          END IF;


          CASE  NONSTAT'( 2 )  IS

               WHEN  2  =>
                    CASE  NONSTAT'( 2 )  IS
                         WHEN  5 | 2..4  =>  NULL;
                    END CASE;  -- ERROR: {2:21;1} MISSING 'OTHERS' II C

               WHEN OTHERS  =>
                    NULL;

          END CASE;


          CASE  NONSTAT( J )  IS

               WHEN  2  =>
                    CASE  NONSTAT( J )  IS
                         WHEN  2..4  =>  NULL;
                         WHEN  1     =>  NULL;
                    END CASE;  -- ERROR: {3:21;1} MISSING 'OTHERS' II D

               WHEN OTHERS  =>
                    NULL;

          END CASE;


     END;    --  STATIC SUBRANGES OF NONSTATIC SUBTYPES


END B54B04B;
