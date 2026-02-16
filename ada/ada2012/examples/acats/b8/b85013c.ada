-- B85013C.ADA

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
--     CHECK THAT A SUBPROGRAM OR ENTRY CAN BE RENAMED WITH:
--       A) DIFFERENT PARAMETER VALUE;
--       B) DIFFERENT DEFAULT VALUES;
--       C) DIFFERENT PARAMETERS HAVING DEFAULT VALUES;
--     AND THAT THE NEW NAMES/DEFAULTS ARE USED WHEN THE NEW NAME IS
--     USED IN A CALL.
--     CASE WHERE THE AGGREGATE CONTEXT IN A RENAMING DECLARATION COMES
--     FROM THE SUBTYPE INDICATIONS IN THE RENAMED SUBPROGRAM,
--     ESPECIALLY FOR A DEFAULT EXPRESSION.

-- HISTORY:
--     RFB 04/06/84
--     EG  05/30/84
--     DWC 10/02/87  SPLIT OUT FUNCTION BODY C1 TO
--                   B85013D.ADA.
--     PWN 10/24/95  REMOVED CHECKS WHERE ADA 95 HAS REMOVED
--                   RESTRICTIONS.
--     PWN 03/28/96  Restored checks in Ada 95 legal format.

PROCEDURE B85013C (N : INTEGER) IS

     I : INTEGER;
     O : STRING(1 .. 1);

     SUBTYPE S  IS STRING;                        -- UNCONSTRAINED.
     SUBTYPE S1 IS STRING(1 .. 2);                -- STATIC.
     SUBTYPE S2 IS STRING(1 .. N);                -- NON-STATIC.

     PROCEDURE P1 (X : S);

     PROCEDURE P2 (X : S1 := (OTHERS => '*'))     -- ERROR: OTHERS NOT
                    RENAMES P1;                   -- ALLOWED SUBTYPE IS
                                                  -- S(TRING) NOT S1.

     PROCEDURE P3 (X : S2 := (OTHERS => '*'));    -- OK - S2 CONSTRAINED

     PROCEDURE P4 (X : S1 := ('1', OTHERS => '*'))-- OK.
                    RENAMES P3;                  

     FUNCTION  F1 (X : S1 := (OTHERS => '1'))     -- ERROR: OTHERS NOT
          RETURN INTEGER RENAMES INTEGER'VALUE;   -- ALLOWED SUBTYPE IS
                                                  -- STRING NOT S1.

     FUNCTION  C1 (L : S1 := (OTHERS => 'L');     -- ERROR: OTHERS NOT
                                                  -- ALLOWED.
                   R : S2) RETURN S RENAMES "&";

     --
     -- DUMMY BODIES FOR P1 AND P3
     --
     PROCEDURE P1 (X : S) IS
     BEGIN
          NULL;
     END;

     PROCEDURE P3 (X : S2 := (OTHERS => '*')) IS  -- OK - S2 CONSTRAINED
     BEGIN
          NULL;
     END;

BEGIN

     P1((OTHERS => '*'));     -- ERROR: S NOT CONSTRAINED => NO OTHERS.

     P3((1 => '1', OTHERS => '*'));   -- OK.

END B85013C;
