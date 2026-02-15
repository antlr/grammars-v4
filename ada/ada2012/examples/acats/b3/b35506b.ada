-- B35506B.ADA

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
-- OBJECTIVE:
--     CHECK THAT THE ATTRIBUTES  T'SUCC ,  T'PRED ,  T'POS , AND
--     T'IMAGE CANNOT INVOLVE FIXED-POINT TYPES (EVEN IF WITH
--     INTEGRAL VALUES).
--     CHECK THAT THE SUBTYPE OF A PARAMETER PASSED TO AN ATTRIBUTE OF
--     A SUBTYPE S BELONGS TO THE BASE TYPE OF S.

-- HISTORY:
--     RM  03/12/81  CREATED ORIGINAL TEST.
--     BCB 08/01/88  MODIFIED HEADER FORMAT AND ADDED CHECKS FOR 'IMAGE.
--     PWN 11/30/94  REMOVED ATTRIBUTE TESTS ILLEGAL IN ADA 9X.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.
--     KAS 12/04/95  ADDED CORRECTED WORDING TO OBJECTIVE

PROCEDURE  B35506B  IS
BEGIN


     DECLARE

          TYPE   FIX_100  IS  DELTA 1.0 RANGE 0.0 .. 100.0 ;
          F100 : FIX_100  :=  10.0 ;
          TYPE   INT_100  IS  NEW INTEGER RANGE 0 .. 100   ;
          I100 : INT_100  :=  10   ;

     BEGIN

          IF  INT_100'SUCC(F100) = INT_100'SUCC(F100)     -- ERROR:
                                        --    NON-DISCRETE ARG.(SUCC)
          THEN  NULL ;
          END IF;

          IF  INT_100'PRED(F100) = INT_100'PRED(F100)     -- ERROR:
                                        --    NON-DISCRETE ARG.(PRED)
          THEN  NULL ;
          END IF;

          I100 := INT_100'POS(F100) ;-- ERROR: NON-DISCRETE ARG.(POS)

          IF INT_100'IMAGE(F100) = INT_100'IMAGE(F100)    -- ERROR:
               THEN NULL;               --    NON-DISCRETE ARG.(IMAGE)
          END IF;

          IF  FIX_100'SUCC(I100) = FIX_100'SUCC(I100)     -- ERROR:
                                        --    NON-DISCRETE TYPE
          THEN  NULL ;
          END IF;

          IF  FIX_100'PRED(I100) = FIX_100'PRED(I100)     -- ERROR:
                                        --    NON-DISCRETE TYPE
          THEN  NULL ;
          END IF;

          IF  FIX_100'POS (I100) = FIX_100'POS (I100)     -- ERROR:
                                        --    NON-DISCRETE TYPE
          THEN  NULL ;
          END IF;


     END ;


END B35506B ;
