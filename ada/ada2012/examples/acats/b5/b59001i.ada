-- B59001I.ADA

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
--    GENERIC SUBPROGRAM OR PACKAGE.

-- SPS 03/08/83
-- EG  10/18/85  CORRECT ERROR COMMENTS

PROCEDURE  B59001I  IS
BEGIN

     << OUTER_LABEL >> NULL;

     DECLARE
          GENERIC
          PROCEDURE PROC;

          GENERIC
          FUNCTION FUN RETURN INTEGER;

          GENERIC
          PACKAGE  PACK  IS

               GENERIC
               PACKAGE  INNER_PACK  IS
                    TYPE  T  IS NEW INTEGER ;
               END  INNER_PACK ;

          END  PACK ;

          PROCEDURE  PROC  IS

               GENERIC
               PROCEDURE INNER_PROC;

               PROCEDURE  INNER_PROC  IS
               BEGIN
                    GOTO  OUTER_LABEL ;          -- ERROR: OUTER LABEL
                    GOTO  OUTER_PROC_LABEL ;     -- ERROR: UNDCL NAME
               END ;

          BEGIN
               << OUTER_PROC_LABEL >> NULL;
               GOTO  OUTER_LABEL ;               -- ERROR: OUTER LABEL
          END ;

          FUNCTION  FUN  RETURN INTEGER  IS

               GENERIC
               FUNCTION INNER_FUN RETURN INTEGER;

               FUNCTION  INNER_FUN  RETURN INTEGER  IS
               BEGIN
                    RETURN  17 ;
                    GOTO  OUTER_LABEL ;          -- ERROR: OUTER LABEL
                    GOTO  OUTER_LABEL_2 ;        -- ERROR: OUTER LABEL
                    GOTO  OUT2_LABEL ;           -- ERROR: UNDCL NAME
                    GOTO  OUTER_FUNC_LABEL ;     -- ERROR: UNDCL NAME
               END  INNER_FUN ;

          BEGIN
               RETURN  19 ;
               << OUTER_FUNC_LABEL >> NULL;
               GOTO  OUTER_LABEL ;               -- ERROR: OUTER LABEL
               GOTO  OUTER_LABEL_2 ;             -- ERROR: OUTER LABEL
               GOTO  OUT2_LABEL ;                -- ERROR: UNDCL NAME
          END  FUN ;

          PACKAGE BODY  PACK  IS

               PACKAGE BODY  INNER_PACK  IS
               BEGIN
                    GOTO  OUTER_LABEL ;          -- ERROR: OUTER LABEL
                    GOTO  OUTER_LABEL_2 ;        -- ERROR: OUTER LABEL
                    GOTO  OUT3_LABEL ;           -- ERROR: UNDCL NAME
                    GOTO  OUTER_PACK_LABEL ;     -- ERROR: UNDCL NAME
               END  INNER_PACK ;

          BEGIN
               << OUTER_PACK_LABEL >> NULL;
               GOTO  OUTER_LABEL ;               -- ERROR: OUTER LABEL
               GOTO  OUTER_LABEL_2 ;             -- ERROR: OUTER LABEL
               GOTO  OUT3_LABEL ;                -- ERROR: UNDCL NAME
          END  PACK ;

     BEGIN

          << OUT2_LABEL >> NULL;
          << OUT3_LABEL >> NULL;

     END ;

     << OUTER_LABEL_2 >> NULL ;

END B59001I;
