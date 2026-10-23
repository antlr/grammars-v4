-- BC1203A.ADA

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
-- CHECK THAT A GENERIC FORMAL TYPE CANNOT BE USED IN ITS OWN
-- ACCESS_TYPE_DEFINITION OR ARRAY_TYPE_DEFINITION.

-- DAT 9/16/81

PROCEDURE BC1203A IS
BEGIN
     DECLARE
          TYPE T2 IS NEW INTEGER;
          TYPE A2 IS ACCESS T2;

          GENERIC
               TYPE D IS ( <> );
               TYPE T1 IS ARRAY (D) OF D;    -- OK.
               TYPE T2 IS ARRAY (D) OF T2;   -- ERROR: T2 CONTAINS T2.
               TYPE AT1 IS ACCESS T1;        -- OK.
               TYPE A2 IS ACCESS A2;         -- ERROR: A2 ACCESSES A2.
          PACKAGE PK IS END PK;
     BEGIN
          NULL;
     END;

END BC1203A;
