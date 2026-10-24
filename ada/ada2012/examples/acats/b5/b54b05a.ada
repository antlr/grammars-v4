-- B54B05A.ADA

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
-- CHECK THAT IF THE CASE EXPRESSION IS  ' I + 0 ' , THE FULL RANGE
--    OF INTEGER VALUES MUST BE COVERED IF  I  IS OF AN INTEGER TYPE
--    OR OF AN INTEGER SUBTYPE.


--     RM  01/30/80
--     SPS 02/08/83
--     RLB 02/08/18 ADDED ERROR LOCATION INDICATORS TO REFLECT COMMON
--                  ERROR REPORTING STRATEGIES.

PROCEDURE  B54B05A  IS
BEGIN

     DECLARE

          SUBTYPE  INT   IS  INTEGER RANGE 1..50 ;
          I   :  INTEGER              := 2 ;
          J   :  INT                  := 2 ;
          K   :  INTEGER RANGE 1..50  := 2 ;
          L   :  INT     RANGE 2..2   := 2 ;

     BEGIN

          CASE  I+0  IS
               WHEN  3 | 5  =>  NULL ;
               WHEN  2 | 4  =>  NULL ;
               WHEN  INTEGER'FIRST..0 | 6..INTEGER'LAST  =>  NULL ;
          END CASE;  -- ERROR: {4:11;1} MISSING 'OTHERS' 1

          CASE  J+0  IS
               WHEN  3 | 5  =>  NULL ;
               WHEN  2 | 4  =>  NULL ;
               WHEN  INTEGER'FIRST..1 |
                     6..50 | 52..INTEGER'LAST  =>  NULL ;
          END CASE;  -- ERROR: {5:11;1} MISSING 'OTHERS' 2

          CASE  J+0  IS
               WHEN  1..50      =>  NULL ;
          END CASE;  -- ERROR: {2:11;1} MISSING 'OTHERS' 3

          CASE  K+0  IS
               WHEN  1..50      =>  NULL ;
          END CASE;  -- ERROR: {2:11;1} MISSING 'OTHERS' 4

          CASE  L+0  IS
               WHEN  1..50      =>  NULL ;
          END CASE;  -- ERROR: {2:11;1} MISSING 'OTHERS' 5

          CASE  K+0  IS
               WHEN  1 | 3 | 5  =>  NULL ;
               WHEN  2 | 4      =>  NULL ;
               WHEN  INTEGER'FIRST..0 | 7..INTEGER'LAST  =>  NULL ;
          END CASE;  -- ERROR: {4:11;1} MISSING 'OTHERS' 6

     END ;


END B54B05A ;
