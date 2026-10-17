-- BD4003B.ADA

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
--     CHECK THAT A RECORD REPRESENTATION SPECIFICATION CANNOT BE
--     GIVEN IN A PACKAGE OR TASK SPECIFICATION FOR A TYPE DECLARED
--     IN AN ENCLOSING DECLARATIVE PART.

-- HISTORY:
--     BCB 03/29/90 CREATED ORIGINAL TEST FROM SPLIT OF BD4003A.ADA.
--     THS 09/24/90 RENAMED TEST FROM '.TST' TO '.ADA'. REMOVED
--                  MACRO ALIGNMENT.

PROCEDURE BD4003B IS

BEGIN

     DECLARE
          TYPE INNER1 IS
               RECORD
                    X : BOOLEAN;
               END RECORD;

          PACKAGE I_P IS
               FOR INNER1 USE                            -- ERROR:
                    RECORD
                         X AT 0 RANGE 0 .. 0;
                    END RECORD;
          END I_P;

          TYPE INNER1_T IS
               RECORD
                    X : BOOLEAN;
               END RECORD;

          TASK I_T IS
               FOR INNER1_T USE                          -- ERROR:
                    RECORD
                         X AT 0 RANGE 0 .. 0;
                    END RECORD;
          END I_T;

          TASK BODY I_T IS
          BEGIN
               NULL;
          END;
     BEGIN
          NULL;
     END;

END BD4003B;
