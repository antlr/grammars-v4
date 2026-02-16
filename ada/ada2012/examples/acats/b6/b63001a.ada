-- B63001A.ADA

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
-- CHECK THAT THE DESIGNATOR AT THE END OF A SUBPROGRAM BODY MUST BE THE
--   SAME AS THE DESIGNATOR USED IN THE SUBPROGRAM SPECIFICATION.

-- DAS  1/23/81

PROCEDURE B63001A IS

     PROCEDURE PROC1 IS
     BEGIN
          NULL;
     END PROC;           -- ERROR: DESIGNATOR DOES NOT MATCH.

     FUNCTION FUNC1 RETURN INTEGER IS
     BEGIN
          RETURN 1;
     END PROC1;          -- ERROR: DESIGNATOR DOES NOT MATCH.

     FUNCTION "+" (X,Y : BOOLEAN) RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END PLUS;           -- ERROR: DESIGNATOR DOES NOT MATCH.

     PROCEDURE PROC2 IS
     BEGIN
          NULL;
     END B63001A.PROC2;  -- ERROR: SELECTED COMPONENT DESIGNATOR.

BEGIN

     NULL;

END B63001A;
