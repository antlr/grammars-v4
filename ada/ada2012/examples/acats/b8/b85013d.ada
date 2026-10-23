-- B85013D.ADA

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
--     DWC 10/02/87  CREATED ORIGINAL TEST FROM SPLIT OF B85013C.ADA.
--     THS 04/12/90  SPLIT THE FUNCTION SPECIFICATION WITH MULTIPLE
--                   ERRORS INTO SEPARATE FUNCTIONS.

PROCEDURE B85013D (N : INTEGER) IS

     SUBTYPE S  IS STRING;                        -- UNCONSTRAINED.
     SUBTYPE S1 IS STRING(1 .. 2);                -- STATIC.
     SUBTYPE S2 IS STRING(1 .. N);                -- NON-STATIC.

     FUNCTION  C1 (L : S1 := (OTHERS => 'L');     -- ERROR: OTHERS NOT
                   R : S2 := "OKAY")              -- ALLOWED.
                         RETURN S RENAMES "&";

     FUNCTION  C2 (L : S1 := "OK";
                   R : S2 := (OTHERS => 'R'))     -- ERROR: OTHERS NOT
                         RETURN S RENAMES "&";    -- ALLOWED.

BEGIN

     NULL;

END B85013D;
