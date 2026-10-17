-- BC1109A.ADA

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
-- CHECK THAT A FORMAL GENERIC OBJECT DECLARATION
--     MAY NOT SPECIFY A RANGE CONSTRAINT.

-- PWB  1/24/86

PROCEDURE BC1109A IS

     TYPE FLOATING IS DIGITS   2 RANGE -1.0 .. 1.0;
     TYPE FIXED    IS DELTA 0.01 RANGE -1.0 .. 1.0;

     GENERIC
          I : INTEGER RANGE 0 .. 10;           -- ERROR: CONSTRAINT.
          B : IN OUT BOOLEAN
                      RANGE FALSE .. TRUE;     -- ERROR: CONSTRAINT.
          C : CHARACTER RANGE 'A' .. 'Z';      -- ERROR: CONSTRAINT.
          FL : IN OUT FLOATING
                       RANGE -1.0 .. 0.0;      -- ERROR: CONSTRAINT.
          FI : FIXED    RANGE -1.0 .. 0.0;     -- ERROR: CONSTRAINT.
     PACKAGE PKG IS
     END PKG;

     GENERIC
          I : IN OUT INTEGER RANGE 0 .. 10;    -- ERROR: CONSTRAINT.
          B : BOOLEAN RANGE FALSE .. TRUE;     -- ERROR: CONSTRAINT.
          C : IN OUT CHARACTER
                      RANGE 'A' .. 'Z';        -- ERROR: CONSTRAINT.
          FL : FLOATING RANGE -1.0 .. 0.0;     -- ERROR: CONSTRAINT.
          FI : IN OUT FIXED RANGE -1.0 .. 0.0; -- ERROR: CONSTRAINT.
     PROCEDURE CHECK ( X : INTEGER );

     PROCEDURE CHECK ( X : INTEGER ) IS
     BEGIN
          NULL;
     END CHECK;

BEGIN     -- BC1109A
     NULL;
END BC1109A;
