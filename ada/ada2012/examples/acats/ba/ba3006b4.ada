-- BA3006B4M.ADA

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
-- CHECK THAT IF A PACKAGE DECLARATION IS SUBSTANTIVELY MODIFIED,
--   PREVIOUSLY COMPILED UNITS USING THE MODIFIED DECLARATIONS
--   MUST BE RECOMPILED, AND ITS BODY BECOMES OBSOLETE.
-- IN THIS TEST, THE PACKAGE BODY IS NOT RECOMPILED.

--SEPARATE FILES ARE:
--   BA3006B0  A LIBRARY PACKAGE DECL AND BODY.
--   BA3006B1  A LIBRARY PROCEDURE.
--   BA3006B2  A LIBRARY PACKAGE DECLARATION (BA3006B0).
--   BA3006B3  SUBUNIT PROCEDURE BODIES (_B0._B30, _B1._B31).
--   BA3006B4M THE MAIN PROCEDURE.

-- WKB 7/8/81
-- JBG 2/13/84
-- JRK 12/7/84
-- WMC 04/07/92 REMOVED UNNECESSARY REFERENCES TO PACKAGE REPORT.

WITH BA3006B1;                -- ERROR: DECLARATION IS OBSOLETE.
PROCEDURE BA3006B4M IS

BEGIN

     BA3006B1;                -- OPTIONAL ERROR.

END BA3006B4M;
