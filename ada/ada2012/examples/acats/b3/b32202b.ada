-- B32202B.ADA

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
--    REQUIRING A FLOATING-POINT VALUE.


-- RM 03/04/81
-- JRK 2/2/83


PROCEDURE  B32202B  IS
BEGIN

     DECLARE

          F1              :  FLOAT  :=  7.0 ;
          TYPE  ACCTYPE  IS  ACCESS FLOAT ;
          ACCOBJ          :  ACCTYPE ;

          TYPE  AGGR     IS
               RECORD
                    X : INTEGER ;
                    Y : FLOAT   ;
               END RECORD;

          AGGR1 : AGGR ;

          PSEUDO_FLOAT : CONSTANT  := -2E3 ;

          A3  :  CONSTANT FLOAT    := PSEUDO_FLOAT   ;         -- ERROR:

          FUNCTION  FN( A5 : FLOAT   := PSEUDO_FLOAT   )       -- ERROR:
                                        RETURN  FLOAT    IS
          BEGIN
               RETURN  PSEUDO_FLOAT   ;                        -- ERROR:
          END  FN ;

          PROCEDURE  PROC( A6 : FLOAT   := PSEUDO_FLOAT   ) IS -- ERROR:
          BEGIN
               NULL ;
          END  PROC ;

     BEGIN

          F1 := PSEUDO_FLOAT   ;                               -- ERROR:
          AGGR1 := ( 7 , PSEUDO_FLOAT   ) ;                    -- ERROR:

          IF  F1 = PSEUDO_FLOAT    THEN                        -- ERROR:
               NULL ;
          END IF;

          PROC( PSEUDO_FLOAT   );                              -- ERROR:

          ACCOBJ  :=  NEW FLOAT ' ( PSEUDO_FLOAT   );          -- ERROR:

     END ;


END B32202B ;
