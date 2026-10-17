-- B83A01C.ADA

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
-- CHECK THAT A STATEMENT LABEL INSIDE AN ACCEPT STATEMENT CANNOT BE
--    THE SAME AS A STATEMENT LABEL OUTSIDE IT.

-- RM  02/05/80
-- EG  10/18/85  CORRECT ERROR COMMENTS.

-- TYPE OF ERRORS:
--         DUPL.1 : ILLEGAL REDECLARATION IN SAME SEQ. OF DECLARATIONS
--         DUPL.2 : LABEL NOT DISTINCT

PROCEDURE  B83A01C  IS

     TASK  TASK1  IS
          ENTRY  E1 ;
          ENTRY  E2 ;
     END  TASK1 ;

     TASK BODY  TASK1  IS
     BEGIN

          << LAB_OUTSIDE_INACCEPT >>             NULL ;

          BEGIN

               << LAB_INBLOCK_INACCEPT >>        NULL ;

               FOR  I  IN  1..2  LOOP
                    << LAB_INBLOCKLOOP_INACCEPT>>NULL ;
               END LOOP;

          END ;

          FOR  I  IN  INTEGER  LOOP
               << LAB_INLOOP_INACCEPT >>         NULL ;
          END LOOP;

          ACCEPT  E1  DO
               << LAB_OUTSIDE_INACCEPT >>        NULL ;-- ERROR: DUPL.1
               << LAB_INBLOCK_INACCEPT >>        NULL ;-- ERROR: DUPL.2
               << LAB_INBLOCKLOOP_INACCEPT>>     NULL ;-- ERROR: DUPL.2
               << LAB_INLOOP_INACCEPT >>         NULL ;-- ERROR: DUPL.1
               << LAB_INACCEPT_INACCEPT >>       NULL ;
          END  E1 ;

          ACCEPT  E2  DO
               << LAB_INACCEPT_INACCEPT >>       NULL ;-- ERROR: DUPL.1
          END  E2 ;

     END  TASK1 ;

BEGIN

     NULL ;

END B83A01C ;
