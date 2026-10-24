-- B45625A.ADA

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
-- CHECK THAT THE EXPONENT FOR INTEGER EXPONENTIATION CANNOT HAVE A TYPE
-- DERIVED FROM INTEGER OR ANY OTHER USER-DEFINED TYPE.

-- CHECK WHEN THE BASE HAS A FLOATING POINT TYPE.

-- JBG 6/1/85

WITH SYSTEM;
PROCEDURE B45625A IS

     TYPE DER_INT IS NEW INTEGER;
     TYPE NEW_INT IS RANGE INTEGER'FIRST .. INTEGER'LAST;
     TYPE SHT_INT IS RANGE 1..10;
     TYPE LNG_INT IS RANGE -SYSTEM.MAX_INT .. SYSTEM.MAX_INT;

     VAR_FLOAT : FLOAT := 1.0;
     V_INTEGER : INTEGER := 1;
     V_DER_INT : DER_INT := 1;
     V_NEW_INT : NEW_INT := 1;
     V_SHT_INT : SHT_INT := 1;
     V_LNG_INT : LNG_INT := 1;

BEGIN

     VAR_FLOAT := VAR_FLOAT ** V_INTEGER;    -- OK.
     VAR_FLOAT := VAR_FLOAT ** V_DER_INT;    -- ERROR: EXP NOT INTEGER.
     VAR_FLOAT := VAR_FLOAT ** V_NEW_INT;    -- ERROR: EXP NOT INTEGER.
     VAR_FLOAT := VAR_FLOAT ** V_SHT_INT;    -- ERROR: EXP NOT INTEGER.
     VAR_FLOAT := VAR_FLOAT ** V_LNG_INT;    -- ERROR: EXP NOT INTEGER.

END B45625A;
