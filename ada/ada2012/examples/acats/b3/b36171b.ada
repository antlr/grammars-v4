-- B36171B.ADA

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
-- CHECK THAT AN UNCONSTRAINED ARRAY TYPE_MARK IS NOT ALLOWED AS THE
-- COMPONENT TYPE IN AN ARRAY_TYPE_DEFINITION USED IN A
-- GENERIC_TYPE_DEFINITION.

-- DAT 2/11/81
-- SPS 12/10/82

PROCEDURE B36171B IS

     TYPE I_1 IS NEW INTEGER RANGE 1 .. 1;
     TYPE U_I IS ARRAY (I_1 RANGE <> ) OF I_1;
     TYPE E_1 IS (ENUM_VALUE);
     TYPE U_E IS ARRAY (E_1 RANGE <> ) OF E_1;

     GENERIC
          TYPE C IS PRIVATE;
          TYPE EA_1 IS ARRAY (E_1 RANGE <> )
               OF C;                         -- OK.
          TYPE EA_2 IS ARRAY (E_1 RANGE <> )
               OF U_E;                       -- ERROR: UNCONSTRAINED.
          TYPE EA_3 IS ARRAY (I_1 RANGE <> )
               OF U_I;                       -- ERROR: UNCONSTRAINED.
          TYPE EA_4 IS ARRAY (E_1) OF C ;    -- OK.
          TYPE EA_5 IS ARRAY (E_1) OF U_E;   -- ERROR: UNCONSTRAINED.
          TYPE EA_6 IS ARRAY (I_1) OF U_I;   -- ERROR: UNCONSTRAINED.

     PROCEDURE G1;

     PROCEDURE G1 IS
     BEGIN
          NULL;
     END;

BEGIN
     NULL;
END B36171B;
