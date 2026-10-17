-- B83003B0M.ADA

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

PROCEDURE B83003B0M IS
     TYPE FMLY IS (FE1, FE2, FE3);

-- MULTIPLE TASKS ARE USED SO THAT ONLY ONE DECLARATION WHICH REQUIRES
-- A BODY (TASK AND GENERIC UNITS) IS GIVEN IN EACH TASK.

-- TASK DECLARATIONS COME FIRST.

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

     TASK TYPE TSK2 IS
          ENTRY E9;
     END TSK2;

     TASK TSK3 IS
          ENTRY E10 (FMLY);
     END TSK3;

     TASK TYPE TSK4 IS
          ENTRY E11;
          ENTRY E12 (FMLY);
     END TSK4;


-- TASK BODIES FOLLOW.

     TASK BODY TSK1 IS SEPARATE;
     TASK BODY TSK2 IS SEPARATE;
     TASK BODY TSK3 IS SEPARATE;
     TASK BODY TSK4 IS SEPARATE;
BEGIN
     NULL;
END B83003B0M;
