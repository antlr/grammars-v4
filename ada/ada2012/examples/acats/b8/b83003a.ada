-- B83003A.ADA

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
--     CHECK THAT AN EXPLICIT DECLARATION IN THE DECLARATIVE PART OF A
--     TASK'S BODY CANNOT HAVE THE SAME IDENTIFIER AS THAT OF A SINGLE
--     ENTRY OR AN ENTRY FAMILY.
--     THIS TEST CHECKS THE CASE WHERE THE TASK SPECIFICATION AND BODY
--     ARE IN THE SAME COMPILATION UNIT.

-- CHANGE HISTORY:
--     VCL  02/04/88  CREATED ORIGINAL TEST.
--     RLB  04/26/21  Corrected incorrect error markers; moved markers on wrong
--                    lines; added location indicators.
--!

PROCEDURE B83003A IS
     TYPE FMLY IS (FE1, FE2, FE3);

BEGIN

-- MULTIPLE TASKS ARE USED SO THAT ONLY ONE DECLARATION WHICH REQUIRES
-- A BODY (TASK OR GENERIC UNIT) IS GIVEN IN EACH TASK.

     DECLARE
          TASK TSK1 IS
               ENTRY E1;
               ENTRY E2 (FMLY);
               ENTRY E3;
               ENTRY E4 (FMLY);
               ENTRY E5;
               ENTRY E6 (FMLY);
               ENTRY E7;
               ENTRY E8 (FMLY);
          END TSK1;

          TASK BODY TSK1 IS
               TYPE E1 IS                           -- ERROR: HOMOGRAPH. {16}
                    ARRAY (1..15) OF CHARACTER;
               SUBTYPE E2 IS STRING(1..12);         -- ERROR: HOMOGRAPH. {16;1}
               E3 : CONSTANT CHARACTER := 'A';      -- ERROR: HOMOGRAPH. {16;1}
               E4 : CONSTANT := 5.0;                -- ERROR: HOMOGRAPH. {16;1}
               E5  : STRING(1..10);                 -- ERROR: HOMOGRAPH. {16;1}
               E6  : EXCEPTION;                     -- ERROR: HOMOGRAPH. {16;1}

               PACKAGE E7 IS END E7;                -- ERROR: HOMOGRAPH. {16;1}

               TASK E8;                             -- ERROR: HOMOGRAPH. {16;1}

          -- BODY FOR THE ABOVE HOMOGRAPH.

               TASK BODY E8 IS                      -- OPTIONAL ERROR: {16}
               BEGIN                                --  BODY OF AN INVALID
                    NULL;                           --  TASK OBJECT.
               END E8;

          BEGIN
               NULL;
          END TSK1;
     BEGIN
          NULL;
     END;

     DECLARE
          TASK TSK3 IS
               ENTRY E10 (FMLY);
          END TSK3;

          TASK BODY TSK3 IS
               GENERIC                       
               FUNCTION E10 RETURN STRING;      -- ERROR: HOMOGRAPH. {1:16;1}

          -- BODY FOR THE ABOVE HOMOGRAPH.

               FUNCTION E10 RETURN STRING IS    -- OPTIONAL ERROR: {16;1}
               BEGIN                            --  BODY OF AN INVALID
                    RETURN "E10";               --  GENERIC FUNCTION.
               END E10;

          BEGIN
               NULL;
          END TSK3;
     BEGIN
          NULL;
     END;

     DECLARE
          TASK TYPE TSK4 IS
               ENTRY E11;
               ENTRY E12 (FMLY);
          END TSK4;

          TASK BODY TSK4 IS
               GENERIC       
               PROCEDURE E11;                   -- ERROR: HOMOGRAPH. {1:16;1}

               GENERIC                         
               PACKAGE E12 IS END E12;          -- ERROR: HOMOGRAPH. {1:16;1}

          -- BODY FOR THE ABOVE HOMOGRAPH.

               PROCEDURE E11 IS                 -- OPTIONAL ERROR:
               BEGIN                            --  BODY OF AN INVALID
                    NULL;                       --  GENERIC PROCEDURE.
               END E11;

          BEGIN
               NULL;
          END TSK4;
     BEGIN
          NULL;
     END;
END B83003A;
