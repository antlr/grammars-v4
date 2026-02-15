-- B55B12B.ADA

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
--    IN A CASE STATEMENT,  AN 'OTHERS' ALTERNATIVE IS REQUIRED IF
--    ST IS A NON-STATIC SUBTYPE, L AND R ARE STATIC, AND THE
--    ALTERNATIVES DO NOT COVER THE RANGE OF ST'BASE.


--     RM  02/02/80
--     JBG 03/25/82
--     JRK 03/02/83
--     RLB 02/08/18 ADDED ERROR LOCATION INDICATORS TO REFLECT COMMON
--                  ERROR REPORTING STRATEGIES.

PROCEDURE  B55B12B  IS

BEGIN

     DECLARE

          C50 : INTEGER := 50;
          SUBTYPE   DYN    IS  INTEGER RANGE 1..C50;
          TYPE  NEW_DYN    IS  NEW  INTEGER RANGE  1..C50;

          TYPE  ENUMERATION  IS  ( A,B,C,D,E,F,G,H,K,L,M,N );
          CM : ENUMERATION := M;
          SUBTYPE   DYN_E  IS
               ENUMERATION RANGE A..CM;
          CTRUE : BOOLEAN := TRUE;
          SUBTYPE   DYN_B  IS  BOOLEAN RANGE FALSE..CTRUE;
          C_L : CHARACTER := 'L';
          SUBTYPE   DYN_C  IS  CHARACTER RANGE 'A'..C_L;

     BEGIN

          FOR  I  IN  DYN  RANGE  1..5  LOOP

               CASE  I  IS
                    WHEN  1 | 3 | 5  =>  NULL ;
                    WHEN  2 | 4      =>  NULL ;
               END CASE;           -- ERROR: {3:16;1} NON-STATIC SUBTYPE.

          END LOOP;

          FOR  I  IN  NEW_DYN  RANGE  1..5  LOOP

               CASE  I  IS
                    WHEN  1 | 3 | 5  =>  NULL ;
                    WHEN  2 | 4      =>  NULL ;
               END CASE;           -- ERROR: {3:16;1} NON-STATIC SUBTYPE.

          END LOOP;

          FOR  I  IN  REVERSE  DYN RANGE 1..5  LOOP

               CASE  I  IS
                    WHEN  1 | 3 | 5  =>  NULL ;
                    WHEN  2 | 4      =>  NULL ;
               END CASE;           -- ERROR: {3:16;1} NON-STATIC SUBTYPE.

          END LOOP;


          FOR  I  IN  DYN_E  RANGE  A..E  LOOP

               CASE  I  IS
                    WHEN  C..E  =>  NULL ;
                    WHEN  A..B  =>  NULL ;
               END CASE;           -- ERROR: {3:16;1} NON-STATIC SUBTYPE.

          END LOOP;


          FOR  I  IN  DYN_B  RANGE TRUE..TRUE  LOOP

               CASE  I  IS
                    WHEN  TRUE  =>  NULL ;
               END CASE;           -- ERROR: {2:16;1} NON-STATIC SUBTYPE.

          END LOOP;


          FOR  I  IN  DYN_C  RANGE 'A'..'E'  LOOP

               CASE  I  IS
                    WHEN  'A'..'C'  =>  NULL ;
                    WHEN  'D'..'E'  =>  NULL ;
               END CASE;           -- ERROR: {3:16;1} NON-STATIC SUBTYPE.

          END LOOP;


          FOR  I  IN  DYN_C  RANGE 'E'..'B'  LOOP

               CASE  I  IS
                    WHEN  'D'..'C'  =>  NULL ;
                    WHEN  'E'..'B'  =>  NULL ;
                    WHEN  'F'..'A'  =>  NULL ;
               END CASE;           -- ERROR: {4:16;1} NON-STATIC SUBTYPE.

          END LOOP;


     END ;

END B55B12B ;
