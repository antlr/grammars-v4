-- B57001B.ADA

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
--    SUBPROGRAM OR A PACKAGE.


-- RM 03/21/81
-- SPS 11/3/82
-- RLB 11/21/19  Added error location indicators.

PROCEDURE  B57001B  IS
BEGIN


     BEGIN

          FOR  I  IN  1..11  LOOP

               NULL ;

               DECLARE
                    PROCEDURE  PR  IS
                    BEGIN
                         NULL ;
                         EXIT ;         -- ERROR: ACROSS SUBPROG. WALLS  {26;2}
                    END  PR ;
               BEGIN
                    EXIT ;              -- OK.                           {21;2}
               END;

          END LOOP;


          FOR  I  IN  CHARACTER  LOOP

               NULL ;

               DECLARE
                    FUNCTION  FN  RETURN CHARACTER  IS
                    BEGIN
                         NULL ;
                         EXIT WHEN I='A';-- ERROR: ACROSS SUBPROG. WALLS {26;1}
                         RETURN  'B' ;
                    END  FN ;
               BEGIN
                    EXIT WHEN FN = I ; -- OK.                            {21;2}
               END;

          END LOOP;


          LOOP_ID :
          FOR  I  IN  1..11  LOOP

               NULL ;

               DECLARE

                    PACKAGE  PACK1  IS
                    END  PACK1 ;

                    PACKAGE BODY  PACK1  IS
                    BEGIN
                         LOOP
                              NULL ;
                              EXIT  LOOP_ID ;-- ERROR: ACROSS PACKAGE   {31;2}
                                             -- WALLS
                         END LOOP;
                    END  PACK1 ;

               BEGIN

                    EXIT  LOOP_ID ;     -- OK.                          {21;2}

               END;


               DECLARE

                    PACKAGE  PACK2  IS
                    END  PACK2 ;

                    PACKAGE BODY  PACK2  IS
                    BEGIN
                         NULL ;
                         EXIT ;         -- ERROR: ACROSS PACKAGE WALLS  {26;2}
                    END  PACK2 ;

               BEGIN

                    EXIT  LOOP_ID ;     -- OK.                          {21;2}

               END;


               DECLARE

                    PACKAGE  PACK1  IS
                    END  PACK1 ;

                    PACKAGE BODY  PACK1  IS
                    BEGIN
                         NULL ;
                         EXIT  WHEN I = 7;-- ERROR: ACROSS PACKAGE WALLS {26;1}
                    END  PACK1 ;

               BEGIN

                    EXIT  LOOP_ID ;     -- OK.

               END;


               FOR  C  IN  CHARACTER  LOOP

                    NULL ;

                    DECLARE
                         FUNCTION  FN  RETURN CHARACTER  IS
                         BEGIN
                              LOOP
                                   NULL ;
                                   RETURN  'B' ;
                                   EXIT  LOOP_ID  WHEN
                                      C = 'A' ;  -- ERROR:             {1:36;2}
                                                 --   ACROSS SUBPROG.
                                                 --   WALLS
                              END LOOP;
                         END  FN ;
                    BEGIN
                         EXIT  LOOP_ID  WHEN  FN = C ;      -- OK.      {26}
                    END;

               END LOOP;

          END LOOP  LOOP_ID ;

     END ;


END B57001B ;
