-- B99001A.ADA

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
-- CHECK THAT THE ATTRIBUTE  'TERMINATED'  MAY NOT BE USED FOR
--     A TASK TYPE OR SUBTYPE.

-- RM 5/14/82
-- TBN 12/17/85     RENAMED FROM B99001A-AB.ADA. ADDED CHECK THAT A
--                  TASK SUBTYPE NAME CANNOT BE USED WITHIN THE TASK
--                  BODY AS A PREFIX TO 'TERMINATED.
-- RJW 4/14/86      RENAMED SUBTYPE.

PROCEDURE  B99001A  IS
BEGIN

     ------------------------------------------------------------------

     DECLARE

          TASK TYPE  T_TYPE  IS
               ENTRY  E ;
          END  T_TYPE ;

          SUBTYPE T_SUBTYPE IS T_TYPE;
          T_OBJECT : T_TYPE ;

          TASK BODY  T_TYPE  IS
               BUSY : BOOLEAN := T_TYPE'TERMINATED ;  -- OK.
          BEGIN

               ACCEPT  E ;

               IF T_SUBTYPE'TERMINATED THEN   -- ERROR: SUBTYPE PREFIX.
                    NULL;
               END IF;

          END  T_TYPE ;

     BEGIN

          IF  T_TYPE'TERMINATED  THEN         -- ERROR: TASK TYPE.
               NULL ;
          ELSE
               T_OBJECT.E ;
          END IF;

     END ;

     -------------------------------------------------------------------

END  B99001A ;
