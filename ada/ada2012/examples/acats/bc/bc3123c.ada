-- BC3123C.ADA

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
-- CHECK THAT AN ACTUAL PARAMETER MUST BE PROVIDED WHEN THE DEFAULT FOR
-- THE GENERIC FORMAL PARAMETER OF MODE IN IS MISSING.

-- TBN 12/01/86

PROCEDURE BC3123C IS

     TYPE ENUM IS (I, II, III);

     TYPE REC IS
          RECORD
               A : BOOLEAN := TRUE;
               B : ENUM := I;
          END RECORD;

BEGIN
     DECLARE
          GENERIC
               GEN_INT1 : IN INTEGER := 1;
               GEN_INT2 : IN INTEGER;
               GEN_INT3 : IN INTEGER := GEN_INT2;
          PACKAGE P IS
               PAC_INT1 : INTEGER := GEN_INT1;
               PAC_INT2 : INTEGER := GEN_INT2;
               PAC_INT3 : INTEGER := GEN_INT3;
          END P;

          PACKAGE P1 IS NEW P;                                 -- ERROR:
     BEGIN
          NULL;
     END;

     DECLARE
          GENERIC
               GEN_REC1 : IN REC;
               GEN_REC2 : IN REC;
          PROCEDURE PROC;

          PROCEDURE PROC IS
          BEGIN
               NULL;
          END PROC;

          PROCEDURE PROC1 IS NEW PROC((FALSE, II));            -- ERROR:
     BEGIN
          NULL;
     END;

     DECLARE
          TYPE ARA IS ARRAY (1 .. 2) OF REC;

          GENERIC
               GEN_ARA1 : IN ARA := (1..2 => (FALSE, III));
               GEN_ARA2 : IN ARA;

          FUNCTION FUN RETURN ARA;

          FUNCTION FUN RETURN ARA IS
               FUN_ARA : ARA;
          BEGIN
               RETURN FUN_ARA;
          END FUN;

          FUNCTION NO_FUN IS NEW FUN;                          -- ERROR:
     BEGIN
          NULL;
     END;

     DECLARE
          TYPE ARA IS ARRAY (1 .. 2) OF REC;

          GENERIC
               GEN_ARA1 : IN ARA := (1..2 => (FALSE, III));
               GEN_ARA2 : IN ARA;

          FUNCTION FUN RETURN ARA;

          FUNCTION FUN RETURN ARA IS
               FUN_ARA : ARA;
          BEGIN
               RETURN FUN_ARA;
          END FUN;

          FUNCTION NO_FUN IS
               NEW FUN(GEN_ARA1 => (1..2 => (TRUE, II)));      -- ERROR:
     BEGIN
          NULL;
     END;

     DECLARE
          TYPE ARA IS ARRAY (1 .. 2) OF REC;

          GENERIC
               GEN_ARA1 : IN ARA := (1..2 => (FALSE, III));
               GEN_ARA2 : IN ARA;

          FUNCTION FUN RETURN ARA;

          FUNCTION FUN RETURN ARA IS
               FUN_ARA : ARA;
          BEGIN
               RETURN FUN_ARA;
          END FUN;

          FUNCTION NO_FUN IS NEW FUN((1..2 => (TRUE, II)));    -- ERROR:
     BEGIN
          NULL;
     END;

END BC3123C;
