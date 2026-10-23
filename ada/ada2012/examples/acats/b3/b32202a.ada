-- B32202A.ADA

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
-- CHECK THAT A REAL NUMBER NAME CANNOT BE USED IN A CONTEXT REQUIRING
--    AN INTEGER VALUE.


-- RM 03/04/81
-- VKG 1/6/83


PROCEDURE  B32202A  IS
BEGIN

     DECLARE

          I1              :  INTEGER   :=  7 ;
          TYPE  ACCTYPE  IS  ACCESS INTEGER  ;
          ACCOBJ          :  ACCTYPE ;

          TYPE  AGG      IS
               RECORD
                    X , Y : INTEGER ;
               END RECORD;

          AGGR : AGG := (3,4);
          PSEUDO_INTEGER  :  CONSTANT  :=  5.0 ;

          A1  :  STRING(1..PSEUDO_INTEGER) ;                   -- ERROR:
          A2  :  INTEGER RANGE 1..PSEUDO_INTEGER ;             -- ERROR:
          A3  :  CONSTANT INTEGER  := PSEUDO_INTEGER ;         -- ERROR:
          TYPE AA( A4 : INTEGER := PSEUDO_INTEGER )  IS        -- ERROR:
               RECORD
                    X : INTEGER ;
                    CASE  A4  IS
                         WHEN  PSEUDO_INTEGER =>               -- ERROR:
                              Y : INTEGER ;
                         WHEN  OTHERS =>
                              Z : INTEGER ;
                    END CASE;
               END RECORD;

          FUNCTION  FN( A5 : INTEGER := PSEUDO_INTEGER )       -- ERROR:
                                        RETURN  INTEGER  IS
          BEGIN
               RETURN  PSEUDO_INTEGER ;                        -- ERROR:
          END  FN ;

          PROCEDURE  PROC( A6 : INTEGER := PSEUDO_INTEGER ) IS -- ERROR:
          BEGIN
               NULL ;
          END  PROC ;

     BEGIN

          I1 := PSEUDO_INTEGER ;                               -- ERROR:
          AGGR.X := 7;                                         -- OK.
          AGGR.Y := PSEUDO_INTEGER;                            -- ERROR:

          IF  I1 = PSEUDO_INTEGER  THEN                        -- ERROR:
               NULL ;
          END IF;

          FOR  I  IN  1..PSEUDO_INTEGER  LOOP                  -- ERROR:
               EXIT;
          END LOOP;

          CASE  I1  IS
               WHEN  PSEUDO_INTEGER =>                         -- ERROR:
                    NULL ;
               WHEN  OTHERS =>
                    NULL ;
          END CASE;

          PROC( PSEUDO_INTEGER );                              -- ERROR:

          ACCOBJ  :=  NEW INTEGER'( PSEUDO_INTEGER );          -- ERROR:

     END ;


END B32202A ;
