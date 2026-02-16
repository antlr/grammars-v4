-- B45501A.ADA

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
--     CHECK THAT THE MULTIPLYING OPERATORS, * AND /, ARE NOT
--     PREDEFINED FOR OPERANDS OF DIFFERENT INTEGER TYPES.

-- HISTORY:
--     BCB 01/28/88  CREATED ORIGINAL TEST.

PROCEDURE B45501A IS

     TYPE X IS RANGE 0 .. 10;

     TYPE Y IS RANGE -25 .. 25;

     TYPE NEWINT IS NEW INTEGER;

     A, B, C : X;
     D, E, F : Y;
     G, H, I : INTEGER;
     J, K, L : NEWINT;

BEGIN

     C := A / B;                                         -- OK.
     C := A / D;                                         -- ERROR:
     C := A / G;                                         -- ERROR:
     C := A / J;                                         -- ERROR:
     C := A * B;                                         -- OK.
     C := A * D;                                         -- ERROR:
     C := A * G;                                         -- ERROR:
     C := A * J;                                         -- ERROR:

     F := D / E;                                         -- OK.
     F := D / B;                                         -- ERROR:
     F := D / H;                                         -- ERROR:
     F := D / K;                                         -- ERROR:
     F := D * E;                                         -- OK.
     F := D * B;                                         -- ERROR:
     F := D * H;                                         -- ERROR:
     F := D * K;                                         -- ERROR:

     I := G / H;                                         -- OK.
     I := G / A;                                         -- ERROR:
     I := G / D;                                         -- ERROR:
     I := G / L;                                         -- ERROR:
     I := G * H;                                         -- OK.
     I := G * A;                                         -- ERROR:
     I := G * D;                                         -- ERROR:
     I := G * L;                                         -- ERROR:

     L := J / K;                                         -- OK.
     L := J / C;                                         -- ERROR:
     L := J / F;                                         -- ERROR:
     L := J / I;                                         -- ERROR:
     L := J * K;                                         -- OK.
     L := J * C;                                         -- ERROR:
     L := J * F;                                         -- ERROR:
     L := J * I;                                         -- ERROR:

END B45501A;
