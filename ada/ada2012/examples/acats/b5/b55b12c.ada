-- B55B12C.ADA

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
-- CHECK THAT THE SUBTYPE OF A LOOP PARAMETER IN A LOOP OF THE FORM
--
--                    FOR  I  IN  ST RANGE L..R  LOOP
--
--    IS CORRECTLY DETERMINED SO THAT WHEN THE LOOP PARAMETER IS USED
--    IN A CASE STATEMENT  AN 'OTHERS' ALTERNATIVE IS NOT REQUIRED IF
--    THE CHOICES COVER THE APPROPRIATE RANGE OF SUBTYPE VALUES.

-- CASE  C :
--    L  AND  R  ARE NON-STATIC EXPRESSIONS,  ST  IS A STATIC SUBTYPE.


--     RM  02/03/80
--     SPS 03/02/83
--     JBG 07/01/83
--     RLB 02/08/18 ADDED ERROR LOCATION INDICATORS TO REFLECT COMMON
--                  ERROR REPORTING STRATEGIES.

PROCEDURE  B55B12C  IS

BEGIN

     DECLARE

          SUBTYPE   STAT  IS       INTEGER RANGE 1..10 ;
          TYPE  NEW_STAT  IS  NEW  INTEGER RANGE 1..10 ;

          TYPE  ENUMERATION  IS  ( A,B,C,D,E,F,G,H,K,L,M,N );
          SUBTYPE   STAT_E  IS     ENUMERATION RANGE A..F ;
          SUBTYPE   STAT_B  IS     BOOLEAN RANGE FALSE..FALSE ;
          SUBTYPE   STAT_C  IS     CHARACTER RANGE 'A'..'F' ;

          I_VAR  :  STAT   ;
          D_VAR  :  NEW_STAT ;
          C_VAR  :  STAT_C ;
          B_VAR  :  STAT_B ;
          E_VAR  :  STAT_E ;

     BEGIN

          I_VAR := 5 ;

          FOR  I  IN  STAT  RANGE  1..I_VAR  LOOP

               CASE  I  IS
                    WHEN  INTEGER'FIRST..-8  =>  NULL ;
                    WHEN  -6..INTEGER'LAST   =>  NULL ;
               END CASE;   -- ERROR: {3:16;1} MISSING 'OTHERS'

          END LOOP;

          D_VAR := 5 ;

          FOR  I  IN  NEW_STAT  RANGE  1..D_VAR  LOOP

               CASE  I  IS
                    WHEN  NEW_STAT'BASE'FIRST..-8  =>  NULL ;
                    WHEN  -6..NEW_STAT'BASE'LAST   =>  NULL ;
               END CASE;   -- ERROR: {3:16;1} MISSING 'OTHERS'

          END LOOP;

          FOR  I  IN  NEW_STAT  RANGE  1..D_VAR  LOOP

               CASE  I  IS
                    WHEN  1..10 => NULL;
               END CASE;   -- ERROR: {2:16;1} MISSING 'OTHERS'

          END LOOP;

          FOR  I  IN  INTEGER  RANGE  1..I_VAR  LOOP

               CASE  I  IS
                    WHEN  INTEGER'FIRST..-8  =>  NULL ;
                    WHEN  -6..INTEGER'LAST   =>  NULL ;
               END CASE;   -- ERROR: {3:16;1} MISSING 'OTHERS'

          END LOOP;


          FOR  I  IN  REVERSE  STAT RANGE 1..I_VAR  LOOP

               CASE  I  IS
                    WHEN  INTEGER'FIRST..-8  =>  NULL ;
                    WHEN  -6..INTEGER'LAST   =>  NULL ;
               END CASE;   -- ERROR: {3:16;1} MISSING 'OTHERS'

          END LOOP;


          E_VAR := E ;

          FOR  I  IN  STAT_E  RANGE A..E_VAR  LOOP

               CASE  I  IS

                    WHEN  A..F  =>  NULL ;
                    WHEN  H..N  =>  NULL ;

               END CASE;   -- ERROR: {5:16;1} MISSING 'OTHERS'

          END LOOP;


          B_VAR := FALSE ;

          FOR  I  IN  STAT_B  RANGE FALSE..B_VAR  LOOP

               CASE  I  IS

                    WHEN  FALSE  =>  NULL ;

               END CASE;   -- ERROR: {4:16;1} MISSING 'OTHERS'

          END LOOP;


          C_VAR := 'E' ;

          FOR  I  IN  STAT_C RANGE 'A'..C_VAR  LOOP

               CASE  I  IS
                    WHEN  CHARACTER'FIRST..'F'  =>  NULL ;
                    WHEN  'H'..CHARACTER'LAST   =>  NULL ;
               END CASE;   -- ERROR: {3:16;1} MISSING 'OTHERS'

          END LOOP;


     END ;


END B55B12C ;
