-- B83A06B.ADA

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
-- CHECK THAT A STATEMENT LABEL IN AN EXCEPTION HANDLER CANNOT BE THE
--    SAME AS A BLOCK IDENTIFIER, LOOP IDENTIFIER, VARIABLE, CONSTANT,
--    NAMED LITERAL, SUBPROGRAM, ENUMERATION LITERAL, TYPE,  PACKAGE,
--    OR EXCEPTION DECLARED IN THE ENCLOSING BODY, AND ALSO THAT A
--    STATEMENT LABEL WITHIN A LOOP CANNOT BE THE SAME
--    AS THE NAME OF AN EXCEPTION.

-- RM  02/13/80
-- JBG 08/21/83
-- JRK 02/01/84
-- EG  10/18/85  CORRECT ERROR COMMENTS.

-- TYPE OF ERRORS:
--         DUPL.1 : ILLEGAL REDECLARATION IN SAME SEQ. OF DECLARATION
--         DUPL.2 : LABEL NOT DISTINCT

PROCEDURE  B83A06B  IS

     LAB_VAR            :  INTEGER;
     LAB_CONST          :  CONSTANT INTEGER := 12;
     LAB_NAMEDLITERAL   :  CONSTANT := 13;
     LAB_EXCEPTION_1    :  EXCEPTION;
     LAB_EXCEPTION_2    :  EXCEPTION;
     LAB_EXCEPTION_3    :  EXCEPTION;
     LAB_EXCEPTION_4    :  EXCEPTION;
     TYPE  ENUM  IS        ( AA , BB , LAB_ENUMERAL );
     TYPE  LAB_TYPE  IS    NEW INTEGER;

     PROCEDURE  LAB_PROCEDURE  IS
     BEGIN
          NULL;
     END LAB_PROCEDURE;

     FUNCTION  LAB_FUNCTION  RETURN INTEGER  IS
     BEGIN
          RETURN 7;
     END LAB_FUNCTION;

     PACKAGE  LAB_PACKAGE  IS
          INT : INTEGER;
     END LAB_PACKAGE;

BEGIN

     LAB_BLOCK_1 :
     BEGIN

          << LAB_EXCEPTION_1 >>     NULL;   -- OK.

          LAB_LOOP_1  :
          FOR  I  IN  INTEGER  LOOP
               << LAB_EXCEPTION_2 >>NULL;   -- OK.
          END LOOP LAB_LOOP_1;

     END  LAB_BLOCK_1;

     LAB_LOOP_2  :
     FOR  I  IN  INTEGER  LOOP
          << LAB_EXCEPTION_3 >>     NULL;   -- ERROR: DUPL.1
     END LOOP LAB_LOOP_2;

     LAB_BLOCK_2 :
     BEGIN
          NULL;
     END LAB_BLOCK_2;

EXCEPTION

     WHEN  CONSTRAINT_ERROR  =>

          << LAB_NAMEDLITERAL >>    NULL;   -- ERROR: DUPL.1
          << LAB_PACKAGE >>         NULL;   -- ERROR: DUPL.1
          << LAB_LOOP_1 >>          NULL;   -- ERROR: DUPL.2
          << LAB_LOOP_2 >>          NULL;   -- ERROR: DUPL.1
          << LAB_CONST >>           NULL;   -- ERROR: DUPL.1
          << LAB_TYPE >>            NULL;   -- ERROR: DUPL.1
          << LAB_FUNCTION >>        NULL;   -- ERROR: DUPL.1
          << LAB_BLOCK_1 >>         NULL;   -- ERROR: DUPL.1
          << LAB_BLOCK_2 >>         NULL;   -- ERROR: DUPL.1
          << LAB_VAR >>             NULL;   -- ERROR: DUPL.1
          << LAB_ENUMERAL >>        NULL;   -- ERROR: DUPL.1
          << LAB_PROCEDURE >>       NULL;   -- ERROR: DUPL.1

          << LAB_EXCEPTION_4 >>     NULL;   -- ERROR: DUPL.1

END B83A06B;
