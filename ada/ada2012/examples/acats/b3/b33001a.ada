-- B33001A.ADA

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
--
--     Check error detection for circular type declarations with
--     private types.
--
-- PASS/FAIL CRITERIA:
--     This test contains several lines marked POSSIBLE ERROR: [Setnn].
--     For each value of n, the implementation must detect one or more of
--     these possible errors. For instance, an error must be detected on
--     at least one of the lines labeled POSSIBLE ERROR: [Set1] for an
--     implementation to pass.
--
--
-- CHANGE HISTORY:
-- RFB 05/24/84
-- EG  05/30/84
-- JWC 10/4/85 RENAMED FROM B33006A-B.ADA; ADDED MAIN PROCEDURE.
-- RLB 11/19/19  Added error location indicators. Added additional
--               possible error locations. Removed unused subprogram, renamed
--               package.

PACKAGE B33001A IS

     SUBTYPE POS IS INTEGER RANGE 1 .. 100;

     TYPE T0A IS PRIVATE;
     TYPE T0B IS PRIVATE;
     TYPE T0C IS PRIVATE;

     TYPE T1A IS PRIVATE;
     TYPE T1B IS PRIVATE;

     TYPE T2A IS PRIVATE;
     TYPE T2B IS PRIVATE;

     TYPE T4A IS PRIVATE;
     TYPE T4B IS PRIVATE;
     TYPE T4C (B : BOOLEAN := FALSE) IS PRIVATE;

     TYPE T5 IS PRIVATE;

     PACKAGE INNER IS

          TYPE IT IS RECORD
               C : T5;                       -- POSSIBLE ERROR: [Set3] {1:11}
          END RECORD;

     END INNER;
     USE INNER;

PRIVATE

     TYPE T0A IS ARRAY (POS) OF T0B;         -- ALL OK DESPITE ORDER.
     TYPE T0B IS RECORD
          C : T0C;
     END RECORD;
     TYPE T0C IS RANGE 1 .. 10;              -- OK.                {4:6}

     TYPE T1A IS ARRAY (POS) OF T1B;
     TYPE T1B IS ARRAY (POS) OF T1A;         -- ERROR: CIRCULAR.   {1:6}

     TYPE T2 IS ARRAY (POS)  OF T2A;         -- OK.                {6}
     TYPE T2A IS RECORD
          C1 : T2B;                          -- POSSIBLE ERROR: [Set1] {1:6}
     END RECORD;

     TYPE T2C IS RECORD
          C2 : T2A;                          -- POSSIBLE ERROR: [Set1] {1:6}
     END RECORD;

     TYPE T2B IS RECORD
          C3 : T2C;                          -- POSSIBLE ERROR: [Set1] {1:6}
     END RECORD;                             -- Circular (T2A, T2B, T2C).

     TYPE T4A IS ARRAY (BOOLEAN) OF T4B;     -- POSSIBLE ERROR: [Set2] {6}
     TYPE T4B IS RECORD
          C1 : T4C;                          -- POSSIBLE ERROR: [Set2] {1:6}
          C2 : INTEGER;
     END RECORD;
     TYPE T4C (B : BOOLEAN := FALSE) IS RECORD -- POSSIBLE ERROR: [Set2] {6}
          CASE B IS
               WHEN TRUE => NULL;
               WHEN FALSE => C5 : T4B;       -- POSSIBLE ERROR: [Set2] {16}
          END CASE;                          -- Circular (T4A, T4B, T4C).
     END RECORD;

     TYPE T5 IS ARRAY (1 .. 2) OF IT;        -- POSSIBLE ERROR: [Set3] {6}
                                             -- Circular (T5, IT).

END B33001A;
