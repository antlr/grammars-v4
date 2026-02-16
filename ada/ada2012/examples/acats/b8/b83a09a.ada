-- B83A09A.ADA

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
--     CHECK THAT IN A NESTED BODY, N, AN ATTEMPT TO REFERENCE AN
--     ENTITY, E, DECLARED IN AN ENCLOSING BODY IS NOT LEGAL IF N
--     CONTAINS A LABEL, BLOCK NAME, OR LOOP NAME E.

-- HISTORY:
--     PMW 09/20/88  CREATED ORIGINAL TEST.

PROCEDURE B83A09A IS

BEGIN
     BEGIN
          DECLARE
               FUNCTION E RETURN BOOLEAN IS
               BEGIN
                    RETURN TRUE;
               END;
          BEGIN
               BEGIN
                    E :
                    LOOP
                         IF E THEN      -- ERROR: SHARED NAME ENTITY E.
                              EXIT E;
                         END IF;
                    END LOOP E;
               END;
          END;
     END;

     BEGIN
          DECLARE
               TYPE STUFF IS (C1, D1, E1, F1);
               ITEM : STUFF := E1;
          BEGIN
               BEGIN
                    <<E1>>
                    IF ITEM = E1 THEN  -- ERROR: SHARED NAME ENTITY E1.
                         NULL;
                    ELSE GOTO E1;
                    END IF;
               END;
          END;
     END;

     BEGIN
          DECLARE
               FUNCTION E2 RETURN BOOLEAN IS
               BEGIN
                    RETURN TRUE;
               END;
          BEGIN
               BEGIN
                    E2 :
                    BEGIN
                         LOOP
                              IF E2 THEN       -- ERROR: SHARED NAME E2.
                                   NULL;
                              END IF;
                         END LOOP;
                    END E2;
               END;
               NULL;
          END;
     END;

     NULL;

END B83A09A;
