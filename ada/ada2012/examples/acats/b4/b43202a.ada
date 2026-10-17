-- B43202A.ADA

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
-- CHECK THAT AN ARRAY AGGREGATE WITH AN OTHERS CHOICE IS
-- ILLEGAL:
--   A) AS AN INITIAL VALUE IN A CONSTANT DECLARATION WHERE
--      THE SUBTYPE INDICATION SPECIFIES AN UNCONSTRAINED
--      ARRAY TYPE.
--   B) IN THE DECLARATION OF A SUBPROGRAM, ENTRY, OR GENERIC
--      FORMAL PARAMETER THAT HAS AN UNCONSTRAINED ARRAY TYPE.
--   C) IN A QUALIFIED EXPRESSION WHEN THE TYPE MARK DENOTES
--      AN UNCONSTRAINED ARRAY TYPE.
--   D) AS AN ACTUAL IN PARAMETER CORRESPONDING TO AN UNCON-
--      STRAINED FORMAL PARAMETER OF A SUBPROGRAM, ENTRY, OR
--      GENERIC UNIT.
--   E) IN A RETURN STATEMENT IN A FUNCTION RETURNING A RESULT
--      OF AN UNCONSTRAINED ARRAY TYPE.

-- EG  01/06/84

PROCEDURE B43202A IS

     TYPE T1 IS ARRAY(INTEGER RANGE <>) OF INTEGER;
     TYPE T2 IS ARRAY(INTEGER RANGE <>, INTEGER RANGE <>) OF INTEGER;

     AA1 : CONSTANT T1 := (-1, 0, 1, OTHERS => 2);      -- ERROR: A.
     BA1, CA1, DA1 : T1(1 .. 5);

     AA2 : CONSTANT T2 := (1 .. 3 => (1, OTHERS => 2)); -- ERROR: A.

     PACKAGE PK IS
          GENERIC
               BA1 : T1 := (-2, OTHERS => -1);          -- ERROR: B.
          PROCEDURE PROC1 (A1 : T1 :=
                       (1, 3, -2, OTHERS => -3));       -- ERROR: B.
     END PK;

     USE PK;

     PACKAGE BODY PK IS
          PROCEDURE PROC1 (A1 : T1 :=
                       (1, 3, -2, OTHERS => -3)) IS     -- ERROR: B.
                       -- AN ERROR NEED NOT BE REPORTED HERE IF NOTED
                       -- AT DECLARATION OF PROCEDURE.
          BEGIN
               NULL;
          END PROC1;
          
          PROCEDURE PROC2 IS
                 NEW PROC1 ((4, 3, 2, OTHERS => 1));    -- ERROR: D.
     BEGIN
          NULL;
     END PK;

     GENERIC
          A2 : T2;
     PACKAGE PKG IS
     END PKG;

     PACKAGE PKG1 IS NEW PKG 
                   ((1 | 3 => (1, 2), OTHERS => (3, 4))); -- ERROR: D.

     TASK TSK1 IS
          ENTRY ENT1 (E1 : T1 := (-2, -3, OTHERS => -1)); -- ERROR: B.
          ENTRY ENT2 (E2 : T2);
     END TSK1;

     TASK BODY TSK1 IS
     BEGIN
          ACCEPT ENT1 (E1 : T1 :=
                            (-2, -3, OTHERS => -1)) DO  -- ERROR: B.
                      -- AN ERROR NEED NOT BE REPORTED HERE IF NOTED
                      -- AT DECLARATION OF ENTRY.
               NULL;
          END ENT1;
     END TSK1;

     FUNCTION FUN1 (A1 : T1) RETURN T1 IS
     BEGIN
          RETURN (2 => 1, OTHERS => -1);                -- ERROR: E.
          RETURN (1, OTHERS => 2);                      -- ERROR: E.
     END FUN1;

     PROCEDURE PROC3 (A1 : T1) IS
     BEGIN
          NULL;
     END PROC3;

BEGIN
     IF ( T1'(1 | 3 | 5 => 2, OTHERS => 1) = CA1 )      -- ERROR: C.
        THEN NULL;
     END IF;
     PROC3 ((4 => -1, 2 => -2, OTHERS => -3));          -- ERROR: D.
     TSK1.ENT1((-5, -4, -3, -2, OTHERS => -1));         -- ERROR: D.
     TSK1.ENT2((1 .. 5 => (1 | 3 => 1, OTHERS => 0)));  -- ERROR: D.
END B43202A;
