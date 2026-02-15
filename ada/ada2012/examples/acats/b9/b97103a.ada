-- B97103A.ADA

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
-- CHECK THAT A  SELECTIVE_WAIT  STATEMENT CANNOT APPEAR OUTSIDE A
--     TASK BODY.  (PART A: PLAIN.)

-- RM 3/23/1982
-- JBG 6/16/83
-- JRK 6/24/86   CLARIFIED ERROR COMMENT.

PROCEDURE  B97103A  IS
BEGIN


     -------------------------------------------------------------------


     DECLARE


          PROCEDURE  B ;


          TASK TYPE  TT  IS
               ENTRY  A ;
          END  TT ;

          TASK BODY  TT  IS
          BEGIN
               NULL ;
          END  TT ;


          TASK TYPE  TT2  IS

               ENTRY  A ;
               ENTRY  B ;

          END  TT2 ;


          TASK BODY  TT2  IS
          BEGIN
               NULL ;
          END  TT2 ;


          PROCEDURE  B  IS
          BEGIN
               NULL ;
          END  B ;


     BEGIN


               SELECT
                    WHEN NOT FALSE =>  -- ERROR: SELECTIVE_WAIT OUTSIDE
                                       --   TASK BODY, OR MISSING
                                       --   ACCEPT_ALTERNATIVE.
                         DELAY 2.5 ;
               OR
                         DELAY 0.0 ;
               OR
                    WHEN TRUE =>
                         DELAY 1.0 ;
               END SELECT;


     END  ;

     -------------------------------------------------------------------


END  B97103A ;
