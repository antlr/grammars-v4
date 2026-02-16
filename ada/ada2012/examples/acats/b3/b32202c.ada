-- B32202C.ADA

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
-- CHECK THAT AN INTEGRAL NUMBER NAME CANNOT BE USED IN A CONTEXT
--    REQUIRING A FIXED-POINT VALUE OR IN A CONTEXT REQUIRING
--    A REAL VALUE.


-- RM 03/04/81
-- SPS 2/10/83

PROCEDURE  B32202C  IS
BEGIN

     DECLARE

          TYPE  MY_FIXED  IS  DELTA 0.5 RANGE 0.0 .. 20.0 ;

          F1               :  MY_FIXED  :=  7.0 ;
          TYPE  ACCTYPE   IS  ACCESS MY_FIXED ;
          ACCOBJ           :  ACCTYPE ;

          TYPE  AGGR     IS
               RECORD
                    X : INTEGER ;
                    Y : MY_FIXED ;
               END RECORD;

          PSEUDO_FIXED    :  CONSTANT  := 12E0 ;
          PSEUDO_REAL1    :  CONSTANT  :=  1E1 ;
          PSEUDO_REAL2    :  CONSTANT  := -2E3 ;
          PSEUDO_REAL3    :  CONSTANT  := +2E3 ;

          REC : AGGR;

          TYPE  MY_OTHER_FIXED  IS
               DELTA  PSEUDO_REAL1     -- ERROR: FIXED OR FLOAT REQUIRED
               RANGE  1.0 .. 10.0;

          TYPE  MY_NEXT_FIXED IS
               DELTA 0.1
               RANGE PSEUDO_REAL2      -- ERROR: FIXED OR FLOAT REQUIRED
                  .. 10.0;

          TYPE MY_LAST_FIXED IS
               DELTA 0.1
               RANGE 0.0
                  ..  PSEUDO_REAL3 ;   -- ERROR: FIXED OR FLOAT REQUIRED

          A3  :  CONSTANT MY_FIXED     :=  PSEUDO_FIXED  ;     -- ERROR:

          FUNCTION  FN( A5 : MY_FIXED  :=  PSEUDO_FIXED  )     -- ERROR:
                                        RETURN  MY_FIXED  IS
          BEGIN
               RETURN  PSEUDO_FIXED ;                          -- ERROR:
          END  FN ;

          PROCEDURE  PROC( A6 : MY_FIXED  := PSEUDO_FIXED ) IS -- ERROR:
          BEGIN
               NULL ;
          END PROC ;

     BEGIN

          F1 := PSEUDO_FIXED ;                                 -- ERROR:
          REC := ( 7 , PSEUDO_FIXED ) ;                       -- ERROR:

          IF  F1 = PSEUDO_FIXED  THEN                          -- ERROR:
               NULL ;
          END IF;

          PROC( PSEUDO_FIXED );                                -- ERROR:

          ACCOBJ  :=  NEW MY_FIXED'(PSEUDO_FIXED );            -- ERROR:

     END ;


END B32202C ;
