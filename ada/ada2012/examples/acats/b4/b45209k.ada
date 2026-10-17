-- B45209K.ADA

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
-- CHECK THAT MEMBERSHIP OPERATIONS ARE NOT DEFINED FOR OPERANDS OF
-- DIFFERENT TYPES.  THIS TEST CHECKS TYPE CLASS PRIVATE.

-- JWC 8/20/85

PROCEDURE B45209K IS

     PACKAGE PKG IS
          TYPE PRIV1 IS PRIVATE;
          TYPE PRIV2 IS PRIVATE;
          C1 : CONSTANT PRIV1;
     PRIVATE
          TYPE PRIV1 IS RANGE 1 .. 10;
          TYPE PRIV2 IS RANGE 1 .. 10;
          C1 : CONSTANT PRIV1 := 1;
     END PKG;

     P : PKG.PRIV1 := PKG.C1;
     B : BOOLEAN;

BEGIN

     B := P IN PKG.PRIV1;          -- OK.

     B := P IN PKG.PRIV2;          -- ERROR: DIFFERENT TYPES.

     B := P NOT IN PKG.PRIV1;      -- OK.

     B := P NOT IN PKG.PRIV2;      -- ERROR: DIFFERENT TYPES.

END B45209K;
