-- B44004A.ADA

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
-- CHECK THAT "OBJECT IN TYPE_MARK RANGE LOW .. HIGH" IS ILLEGAL.

-- PWB  03/04/86

PROCEDURE B44004A IS

     TYPE ENUM IS (EA, EB, EC);
     SUBTYPE SMALL IS INTEGER RANGE 0 .. 10;
     TYPE FLOATING IS DIGITS 5;
     TYPE FIXED IS DELTA 0.1 RANGE 1.0..100.0;

     EVAR : ENUM := EA;
     IVAR : INTEGER := 0;
     CVAR : CHARACTER := 'A';
     BVAR : BOOLEAN := FALSE;
     FIVAR : FIXED := 1.0;
     FLVAR : FLOATING := 0.0;

BEGIN

     BVAR := ( EVAR IN ENUM RANGE EA .. EC );        -- ERROR: RANGE.

     IF IVAR IN SMALL RANGE 1..5 THEN                -- ERROR: RANGE.
          NULL;
     ELSIF IVAR NOT IN INTEGER RANGE 0..10 THEN      -- ERROR: RANGE.
          NULL;
     ELSIF CVAR IN CHARACTER RANGE 'A' .. 'Z' THEN   -- ERROR: RANGE.
          NULL;
     ELSIF BVAR NOT IN 
                BOOLEAN RANGE TRUE .. FALSE THEN     -- ERROR: RANGE.
          NULL;
     END IF;

     IF FIVAR IN FIXED RANGE 2.0 .. 3.0 THEN         -- ERROR: RANGE.
          NULL;
     ELSIF FIVAR NOT IN FIXED RANGE 2.0 .. 3.0 THEN  -- ERROR: RANGE.
          NULL;
     ELSIF FLVAR IN FLOATING RANGE 1.0 .. 2.0 THEN   -- ERROR: RANGE.
          NULL;
     ELSE 
          BVAR := (FLVAR NOT IN 
                   FLOATING RANGE 1.0 .. 2.0);       -- ERROR: RANGE.
     END IF;

END B44004A;
