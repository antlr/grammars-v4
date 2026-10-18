-- BC1103A.ADA

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
-- CHECK THAT GENERIC IN PARAMETERS ACT LIKE CONSTANTS, I.E., THEY
-- CANNOT BE PASSED IN OUT TO SUBPROGRAMS AND GENERICS, OR OUT TO
-- SUBPROGRAMS, CANNOT BE ASSIGNED TO, AND THAT
-- COMPONENTS CANNOT BE ASSIGNED TO.

-- DAT 8/6/81
-- SPS 9/8/82

PROCEDURE BC1103A IS

     TYPE S IS RANGE 1 .. 10;

     TYPE A IS ARRAY (1 .. 2) OF S;

     TYPE R IS RECORD
          C1, C2 : S;
     END RECORD;

     TYPE AA IS ACCESS A;

     GENERIC
          TYPE T IS PRIVATE;
          VT : IN OUT T;
     PROCEDURE GEN;

     PROCEDURE PS (P : OUT S) IS
     BEGIN
          NULL;
     END PS;

     PROCEDURE PS2 (P : IN OUT S) IS
     BEGIN
          NULL;
     END PS2;

     PROCEDURE PA (P : OUT A) IS
     BEGIN
          NULL;
     END PA;

     PROCEDURE PA2 (P : IN OUT A) IS
     BEGIN
          NULL;
     END PA2;

     PROCEDURE PR (P : OUT R) IS
     BEGIN
          NULL;
     END PR;

     PROCEDURE PR2 (P : IN OUT R) IS
     BEGIN
          NULL;
     END PR2;

     PROCEDURE PAA (P : OUT AA) IS
     BEGIN
          NULL;
     END PAA;

     PROCEDURE PAA2 (P : IN OUT AA) IS
     BEGIN
          NULL;
     END PAA2;

     PROCEDURE GEN IS
     BEGIN NULL; END GEN;

BEGIN
     DECLARE
          GENERIC
               VS : S;
               VA : A;
               VR : IN R;
               VAA : AA;
          PACKAGE PKG IS END PKG;

          PACKAGE BODY PKG IS
               PROCEDURE G1 IS NEW 
                    GEN (S, VS);             -- ERROR: VS IS IN PARM.
               PROCEDURE G2 IS NEW GEN
                    (A, VA);                 -- ERROR: VA IS IN PARM.
               PROCEDURE G3 IS NEW GEN
                    (R, VR);                 -- ERROR: VR IS IN PARM.
               PROCEDURE G4 IS NEW GEN
                    (AA, VAA);               -- ERROR: VAA IS IN PARM.
               PROCEDURE G5 IS NEW GEN
                    (A, VAA.ALL);            -- OK.
               PROCEDURE G6 IS NEW GEN
                    (S, VA(1));              -- ERROR: VA IS IN PARM.
               PROCEDURE G7 IS NEW GEN
                    (S, VR.C2);              -- ERROR: VR IS IN PARM.
               PROCEDURE G8 IS NEW GEN
                    (A, VAA(1..2));          -- OK.
               PROCEDURE G9 IS NEW GEN
                    (S, VAA(1));             -- OK.
          BEGIN
               VS := 5;                      -- ERROR: VS IS IN PARM.
               PS (VS);                      -- ERROR: VS IS IN PARM.
               PS2 (VS);                     -- ERROR: VS IS IN PARM.
               VA := (5, 5);                 -- ERROR: VA IS IN PARM.
               PA (VA);                      -- ERROR: VA IS IN PARM.
               PA2 (VA);                     -- ERROR: VA IS IN PARM.
               PA2 (VA (1..2));              -- ERROR: VA IS IN PARM.
               PA2 (VA (1..2)(1..2));        -- ERROR: VA IS IN PARM.
               VA(1) := 5;                   -- ERROR: VA IS IN PARM.
               VA (1..2) (1..2) := (5, 5);   -- ERROR: VA IS IN PARM.
               VA (2..1) := VA (2..1);       -- ERROR: VA IS IN PARM.
               VR := (5, 5);                 -- ERROR: VR IS IN PARM.
               PR (VR);                      -- ERROR: VR IS IN PARM.
               PR2 (VR);                     -- ERROR: VR IS IN PARM.
               VR.C1 := 5;                   -- ERROR: VR IS IN PARM.
               PS (VR.C2);                   -- ERROR: VR IS IN PARM.
               PS2 (VR.C1);                  -- ERROR: VR IS IN PARM.
               PS (VA(1));                   -- ERROR: VA IS IN PARM.
               PS2 (VA(2));                  -- ERROR: VA IS IN PARM.
               VAA := NULL;                  -- ERROR: VAA IS IN PARM.
               VAA := NEW A'(5, 5);          -- ERROR: VAA IS IN PARM.
               VAA.ALL := (5, 5);            -- OK.
               VAA(1) := 5;                  -- OK.
               VAA(1..2) := (5, 5);          -- OK.
               PAA (VAA);                    -- ERROR: VAA IS IN PARM.
               PAA2 (VAA);                   -- ERROR: VAA IS IN PARM.
               PA (VAA.ALL);                 -- OK.
               PA2 (VAA.ALL);                -- OK.
          END PKG;
     BEGIN
          NULL;
     END;
END BC1103A;
