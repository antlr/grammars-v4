-- B59001C.ADA

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
-- CHECK THAT A GOTO STATEMENT CANNOT TRANSFER CONTROL OUT OF A
--    TASK BODY.

-- CHECK THAT A GOTO STATEMENT CANNOT TRANSFER CONTROL OUT OF AN
--    ACCEPT STATEMENT.

-- RM  06/04/81
-- SPS 03/08/83
-- EG  10/18/85  CORRECT ERROR COMMENTS.

PROCEDURE  B59001C  IS
BEGIN

     << OUTER_LABEL >>
     << OUTER_LABEL_1 >>
     << OUTER_LABEL_8 >>

     DECLARE

          TASK  TK  IS
               ENTRY  EN ;
          END  TK ;

          TASK BODY  TK  IS

               TASK  INNER_TK  IS
                    ENTRY  INNER_EN ;
               END  INNER_TK ;

               TASK BODY  INNER_TK  IS
               BEGIN
                    GOTO  OUTER_LABEL ;   -- ERROR: OUT OF TASK BODY
                    << NEWL >>
                    GOTO  OUTER_LABEL_2 ; -- ERROR: OUT OF TASK BODY
                    GOTO  INTERM_LABEL ;  -- ERROR: UNDCL LABEL
                    GOTO  LAB ;           -- ERROR: UNDCL LABEL
                    GOTO  NONEXISTENT1 ;  -- ERROR: NO SUCH LABEL

                    ACCEPT  INNER_EN  DO
                         GOTO  NEWL ;          -- ERROR: OUT OF ACCEPT
                         GOTO  OUTER_LABEL_1 ; -- ERROR: OUT OF ACCEPT
                         GOTO  OUTER_LABEL_3 ; -- ERROR: OUT OF ACCEPT
                         GOTO  INTERM_LABEL_1; -- ERROR: UNDCL LABEL
                         GOTO  LAB_1 ;         -- ERROR: UNDCL LABEL
                         GOTO  NONEXISTENT2 ;  -- ERROR: NO SUCH LABEL
                    END  INNER_EN ;

               END  INNER_TK ;

          BEGIN

               << LAB >>
               << LAB_1 >>
               << LAB_8 >>

               GOTO  OUTER_LABEL_2 ;      -- ERROR: OUT OF TASK BODY
               GOTO  OUTER_LABEL ;        -- ERROR: OUT OF TASK BODY
               GOTO  INTERM_LABEL ;       -- ERROR: UNDCL LABEL
               GOTO  NONEXISTENT3 ;       -- ERROR: NO SUCH LABEL

               ACCEPT  EN  DO
                    GOTO  OUTER_LABEL_8 ;      -- ERROR: OUT OF ACCEPT
                    GOTO  OUTER_LABEL_9 ;      -- ERROR: OUT OF ACCEPT
                    GOTO  INTERM_LABEL_9;      -- ERROR: UNDCL LABEL
                    GOTO  LAB_8 ;              -- ERROR: OUT OF ACCEPT
                    GOTO  LAB_9 ;              -- ERROR: OUT OF ACCEPT
                    GOTO  NONEXISTENT4 ;       -- ERROR: NO SUCH LABEL
               END  EN ;

               << LAB_2 >>
               << LAB_9 >>
               NULL ;

          END  TK ;

     BEGIN

          << INTERM_LABEL >>
          << INTERM_LABEL_1 >>
          << INTERM_LABEL_9 >>
          NULL ;

     END ;

     << OUTER_LABEL_2 >>
     << OUTER_LABEL_3 >>
     << OUTER_LABEL_9 >>
     NULL ;

END B59001C;
