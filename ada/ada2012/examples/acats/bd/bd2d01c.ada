-- BD2D01C.ADA

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
--     CHECK THAT A 'SMALL SPECIFICATION CANNOT BE GIVEN FOR A NAME
--     DECLARED BY AN OBJECT OR A SUBTYPE DECLARATION.

-- HISTORY:
--     BCB 04/05/88  CREATED ORIGINAL TEST.

PROCEDURE BD2D01C IS

     CHECK_SMALL : CONSTANT := 0.25;

     TYPE FIX IS DELTA 2.0**(-1) RANGE -1.0 .. 1.0;

     TYPE FX IS DELTA 2.0**(-1) RANGE -1.0 .. 1.0;
     SUBTYPE SFX IS FX;

     F1 : FIX;

     FOR F1'SMALL USE CHECK_SMALL;            -- ERROR: OBJECT.
     FOR SFX'SMALL USE CHECK_SMALL;           -- ERROR: SUBTYPE.

BEGIN
     NULL;
END BD2D01C;
