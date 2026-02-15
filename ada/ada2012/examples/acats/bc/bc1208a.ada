-- BC1208A.ADA

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
--     CHECK THAT THE DISCRIMINANTS OF A FORMAL GENERIC PARAMETER MUST
--     ALL HAVE DIFFERENT IDENTIFIERS.

-- HISTORY:
--     BCB 03/29/88  CREATED ORIGINAL TEST.
--     THS 03/30/90  MOVED "-- ERROR:" TO THE RIGHT OF THE CONSTRUCT
--                   IN ERROR.  REMOVED 'OK' CASE.

PROCEDURE BC1208A IS

     GENERIC
          TYPE DTYPE2 (D1 : INTEGER; D1 : BOOLEAN) IS PRIVATE; -- ERROR:
                                    -- TWO DISCRIMINANTS WITH
                                    -- THE SAME IDENTIFIER.

          TYPE DTYPE3 (D2 : BOOLEAN; D2 : BOOLEAN) IS PRIVATE; -- ERROR:
                                    -- TWO DISCRIMINANTS WITH
                                    -- THE SAME IDENTIFIER.
     PACKAGE P IS
     END P;

BEGIN
     NULL;
END BC1208A;
