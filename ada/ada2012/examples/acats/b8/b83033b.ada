-- B83033B.ADA

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
--     CHECK THAT AN IMPLICIT DECLARATION OF A BLOCK NAME, A LOOP NAME,
--     OR A STATEMENT LABEL HIDES THE DECLARATION OF AN ENUMERATION
--     LITERAL OR OF A DERIVED SUBPROGRAM DECLARED BY A DERIVED TYPE
--     DEFINITION, SO THAT THE USE OF THE COMMON IDENTIFIER MUST
--     BE REJECTED IF IT WOULD BE LEGAL FOR THE HIDDEN LITERAL OR
--     SUBPROGRAM BUT AN ILLEGAL REFERENCE TO THE IMPLICITLY DECLARED
--     NAME OR LABEL.

-- HISTORY:
--     DHH 09/21/88  CREATED ORIGINAL TEST.
--     WMC 03/21/92  REDUCED TEST REDUNDANCIES.


PROCEDURE B83033B IS
     PACKAGE GEN_P IS
          TYPE A IS (RED, YELO, GREEN);
          FUNCTION NEXT(X : A) RETURN A;
          FUNCTION BLUE(X : A) RETURN A;
     END GEN_P;

     PACKAGE BODY GEN_P IS
          FUNCTION NEXT(X : A) RETURN A IS
          BEGIN
               RETURN X;
          END NEXT;

          FUNCTION BLUE(X : A) RETURN A IS
          BEGIN
               RETURN X;
          END BLUE;

     END GEN_P;

BEGIN

B1:  DECLARE
          TYPE STMT2 IS NEW GEN_P.A;
          C : STMT2;
     BEGIN

--             DEMONSTRATE THAT STATEMENT LABEL RED HIDES ENUMERATION LITERAL.
               C := NEXT(RED);                                -- ERROR:

--             DEMONSTRATE THAT LOOP NAME BLUE HIDES DERIVED SUBPROGRAM.
               C := BLUE(GREEN);                              -- ERROR:

--             DEMONSTRATE THAT BLOCK NAME YELO HIDES ENUMERATION LITERAL.
               C := NEXT(YELO);                               -- ERROR:

     <<RED>>   NULL;

               BLUE:
               FOR I IN 1 .. 1 LOOP
                    NULL;
               END LOOP BLUE;

     YELO:     DECLARE
               BEGIN
                    NULL;
               END YELO;
     END B1;

END B83033B;
