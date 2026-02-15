-- B99003A.ADA

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
-- CHECK THAT A COUNT ATTRIBUTE FOR AN ENTRY OF A FAMILY THE ENTRY NAME
--     MUST HAVE THE FORM OF AN INDEXED COMPONENT WITH ONE AND ONLY ONE
--     INDEX.


-- RM 5/19/82


PROCEDURE  B99003A  IS
BEGIN


     -------------------------------------------------------------------


     DECLARE


          TASK TYPE  T_TYPE  IS
               ENTRY  E ( BOOLEAN );
          END  T_TYPE ;


          T_OBJECT1 : T_TYPE ;
          T_OBJECT2 : T_TYPE ;


          TASK BODY  T_TYPE  IS
               BUSY : BOOLEAN := FALSE ;
          BEGIN

               IF  E'COUNT = 0  THEN        -- ERROR: INDEX MISSING.
                    NULL;
               END IF;

               IF  E( TRUE OR FALSE ) 'COUNT = 0  THEN     -- OK.
                    NULL;
               END IF;

               IF  E( TRUE , FALSE ) 'COUNT = 0   -- ERROR: 2 INDICES.
               THEN
                    NULL;
               END IF;

          END  T_TYPE ;


     BEGIN

          NULL;

     END ;


     -------------------------------------------------------------------


END  B99003A ;
