-- BC1306A.ADA

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
-- CHECK THAT A GENERIC FORMAL SUBPROGRAM CANNOT BE USED IN A 
-- CONDITIONAL OR TIMED ENTRY CALL, EVEN IF NEVER INSTANTIATED.

-- DAT 8/27/81

PROCEDURE BC1306A IS

     TASK TYPE T IS
          ENTRY ENT;
     END T;

     TASK BODY T IS
     BEGIN
          SELECT
               ACCEPT ENT;
          OR
               TERMINATE;
          END SELECT;
     END T;

     PACKAGE Q IS
          TT : T;
     END Q;
     USE Q;
     PACKAGE BODY Q IS
     BEGIN NULL; END Q;

     GENERIC
          WITH PROCEDURE ENT;
     PACKAGE PK IS END PK;

     PACKAGE BODY PK IS
     BEGIN
          ENT;                          -- OK.
          SELECT
               ENT;                     -- ERROR: GENERIC PARM.
          ELSE 
               NULL;
          END SELECT;
          SELECT
               TT.ENT;                  -- OK.
          ELSE
               NULL;
          END SELECT;
          SELECT
               ENT;                     -- ERROR: GENERIC PARM.
          OR
               DELAY 1.0;
          END SELECT;
     END PK;

BEGIN
     NULL;
END BC1306A;
