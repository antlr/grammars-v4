-- B45601A.ADA

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
-- CHECK THAT FIXED POINT VALUES CAN NOT BE EXPONENTIATED.  INCLUDES 
-- CHECK THAT REAL EXPONENTS ARE NOT ALLOWED.

-- RJW 2/26/86
-- JRL 09/29/96  Changed initialization of object F to value guaranteed to
--               be in the base range of the type FIXED. Changed delta of
--               type FIXED from 1.0 to 0.1.

PROCEDURE B45601A IS

     TYPE FIXED IS DELTA 0.1 RANGE -1.0 .. 1.0;

     F : FIXED := 0.0;

     G : FLOAT := 1.0;

BEGIN
     F := F ** 2;               -- ERROR: ** NOT ALLOWED FOR FIXED.
     F := F ** 1;               -- ERROR: ** NOT ALLOWED FOR FIXED.
     F := F ** 0;               -- ERROR: ** NOT ALLOWED FOR FIXED.

     F := F ** 1.0;             -- ERROR: ** NOT ALLOWED FOR FIXED.
     F := F ** F;               -- ERROR: ** NOT ALLOWED FOR FIXED.

     G := G ** F;               -- ERROR: REAL EXPONENT NOT ALLOWED.
     G := G ** 1.0;             -- ERROR: REAL EXPONENT NOT ALLOWED.
     G := G ** G;               -- ERROR: REAL EXPONENT NOT ALLOWED.
END B45601A;
