-- BA3006A6M.ADA

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
--   MUST BE RECOMPILED, AS MUST ITS BODY.
-- IN THIS TEST, A PROCEDURE, FUNCTION, PACKAGE DECLARATION, AND
--   PACKAGE BODY UNIT ARE NOT RECOMPILED.

-- SEPARATE FILES ARE:
--   BA3006A0  A LIBRARY PACKAGE DECLARATION.
--   BA3006A1  A LIBRARY PROCEDURE DECL AND BODY.
--   BA3006A2  A LIBRARY FUNCTION.
--   BA3006A3  A LIBRARY PACKAGE DECLARATION.
--   BA3006A4  A LIBRARY PACKAGE DECL AND BODY.
--   BA3006A5  A LIBRARY PACKAGE DECLARATION (BA3006A0).
--   BA3006A6M THE MAIN PROCEDURE AND SUBUNIT BODIES (_A1._A7, _A2._A8,
--                                                    _A4._A9).

-- WKB 7/8/81
-- BHS 8/14/84
-- JRK 12/7/84


WITH BA3006A1;                -- OK: ONLY BODY IS OBSOLETE.
WITH BA3006A2;                -- ERROR: DECLARATION IS OBSOLETE.
WITH BA3006A3;                -- ERROR: DECLARATION IS OBSOLETE.
WITH BA3006A4;                -- OK: ONLY BODY IS OBSOLETE.
PROCEDURE BA3006A6M IS
BEGIN

     BA3006A1;                -- OK: BODY NOT NEEDED YET.
     BA3006A4.J := 0;         -- OK: BODY NOT NEEDED YET.

END BA3006A6M;


SEPARATE (BA3006A1)           -- ERROR: OBSOLETE.
PROCEDURE BA3006A7 IS
BEGIN
     NULL;
END BA3006A7;


SEPARATE (BA3006A2)           -- ERROR: OBSOLETE.
PROCEDURE BA3006A8 IS
BEGIN
     NULL;
END BA3006A8;


SEPARATE (BA3006A4)           -- ERROR: OBSOLETE.
PROCEDURE BA3006A9 IS
BEGIN
     NULL;
END BA3006A9;
