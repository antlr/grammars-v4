-- B57001C.ADA

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
-- CHECK THAT AN EXIT STATEMENT CANNOT TRANSFER CONTROL OUTSIDE A
--    TASK BODY OR OUTSIDE AN ACCEPT STATEMENT.


-- RM 03/21/81
-- SPS 3/7/83

PROCEDURE  B57001C  IS
BEGIN

     BEGIN

          FOR  I  IN  1..11  LOOP

               NULL ;

               DECLARE

                    TASK  TK ;

                    TASK BODY  TK  IS
                    BEGIN
                         NULL ;
                         EXIT ;         -- ERROR: ACROSS TASK BODY WALLS
                    END  TK ;

               BEGIN
                    EXIT ;              -- OK.
               END;

          END LOOP;


          OUTER_L :
          FOR  I  IN  CHARACTER  LOOP

               NULL ;

               DECLARE

                    TASK  TK   IS
                         ENTRY  E1 ;
                         ENTRY  E2 ;
                    END  TK ;

                    TASK BODY  TK  IS
                    BEGIN
                         LOOP_ID :
                         LOOP
                              SELECT

                                   ACCEPT  E1  DO
                                        EXIT  WHEN  I = 'B' ; -- ERROR:
                                        --    ACROSS 'ACCEPT STATEMENT'
                                        --    WALLS.
                                   END  E1 ;

                                   EXIT  WHEN  I= 'B' ;       -- OK.

                              OR

                                   ACCEPT  E2  DO
                                        EXIT  LOOP_ID ;        -- ERROR:
                                        --    ACROSS  'ACCEPT STATEMENT'
                                        --    WALLS.
                                   END  E2 ;

                                   EXIT LOOP_ID  WHEN I = 'B' ;-- OK.
                                   EXIT OUTER_L  WHEN I = 'B' ;-- ERROR:
                                   --    ACROSS TASK BODY WALLS


                              END SELECT;
                         END LOOP  LOOP_ID ;
                         EXIT ;         -- ERROR: ACROSS TASK BODY WALLS
                    END  TK ;

               BEGIN
                    EXIT ;              -- OK.
               END;

          END LOOP  OUTER_L ;


     END ;


END B57001C ;
