-- B95063A.ADA

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
-- CHECK THAT DEFAULT EXPRESSIONS ARE FORBIDDEN FOR FORMAL PARAMETERS
-- OF MODE IN OUT OR OUT.

-- JWC 7/19/85

PROCEDURE B95063A IS

     TASK T IS

          ENTRY E1 ( X : IN OUT INTEGER := 1);   -- ERROR: IN OUT.

          ENTRY E2 ( X : OUT INTEGER := 1);      -- ERROR: OUT.

          ENTRY E3 ( X : IN INTEGER := 1);       -- OK: IN.

     END T;

     TASK BODY T IS
     BEGIN
          NULL;
     END T;

BEGIN
     NULL;
END B95063A;
