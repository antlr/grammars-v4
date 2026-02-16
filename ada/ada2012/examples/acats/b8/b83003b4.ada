-- B83003B4.ADA

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
--     ARE IN SEPARATE COMPILATIONS (TASKS BODIES AS SUBUNITS).

-- SEPARATE FILES:
--     B83003B0M     CONTAINS THE DECLARATIONS OF THE TASKS AND THE
--                   TASK BODY STUBS.
--
--     B83003B1      CONTAINS SEPARATE TASK BODIES.
--
--     B83003B2      CONTAINS SEPARATE TASK BODIES.
--
--     B83003B3      CONTAINS SEPARATE TASK BODIES.
--
--     B83003B4      CONTAINS SEPARATE TASK BODIES.

-- HISTORY:
--     VCL  02/04/88  CREATED ORIGINAL TEST.
--     RDH  04/09/90  MODIFIED TEST TO ONLY HAVE ONE TASK BODY IN
--                    EACH FILE.
--     RLB  04/26/21  Corrected incorrect error markers; moved markers on wrong
--                    lines; added location indicators.
--!

-- MULTIPLE TASKS ARE USED SO THAT ONLY ONE DECLARATION WHICH REQUIRES
-- A BODY (TASK AND GENERIC UNITS) IS GIVEN IN EACH TASK.

SEPARATE (B83003B0M)
TASK BODY TSK4 IS
     GENERIC                                      
     PROCEDURE E11;                               -- ERROR: HOMOGRAPH. {1:6;1}

     GENERIC                                      
     PACKAGE E12 IS END E12;                      -- ERROR: HOMOGRAPH. {1:6;1}

     -- BODY FOR THE ABOVE HOMOGRAPH.

     PROCEDURE E11 IS                             -- OPTIONAL ERROR: {6}
     BEGIN                                        --  BODY OF AN INVALID
          NULL;                                   --  GENERIC PROCEDURE.
     END E11;
BEGIN
     NULL;
END TSK4;
