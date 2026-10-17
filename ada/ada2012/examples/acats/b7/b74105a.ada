-- B74105A.ADA

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
-- CHECK THAT THE FULL TYPE DECLARATION OF A PRIVATE TYPE MAY NOT BE: 
   -- (1) AN UNCONSTRAINED TYPE WITH DISCRIMINANTS.
   -- (2) AN UNCONSTRAINED ARRAY TYPE.

-- DSJ 4/29/83
-- SPS 10/20/83
-- PWN 11/09/95  REMOVED CHECKS WHERE CONSTRAINT RULES RELAXED.
-- PWN 03/21/96  Restored checks in Ada 95 legal format.

PROCEDURE B74105A IS

     PACKAGE PACK1 IS
          TYPE UNCONS4 IS PRIVATE;
          TYPE UNCONS5 IS PRIVATE;
          TYPE UNCONS6 IS PRIVATE;
          TYPE UNCONS7 IS PRIVATE;
          TYPE UNCONS8 IS PRIVATE;
     PRIVATE
          TYPE UNCONS1 (H : INTEGER) IS
               RECORD
                    NULL;
               END RECORD;
          TYPE UNCONS2 (D : INTEGER := 2) IS
               RECORD
                    NULL;
               END RECORD;
          TYPE UNCONS3 IS ARRAY
                    (INTEGER RANGE <>) OF INTEGER;
          TYPE UNCONS4 IS NEW UNCONS1;       -- ERROR: UNCONSTRAINED
          TYPE UNCONS5 IS NEW UNCONS2;       -- OK.
          TYPE UNCONS6 IS NEW UNCONS3;       -- ERROR: UNCONSTRAINED
          TYPE UNCONS7 IS ARRAY              -- ERROR: UNCONSTRAINED
                    (INTEGER RANGE <>) OF INTEGER;
          TYPE UNCONS8 (D:INTEGER:=2) IS     -- OK.
               RECORD
                    NULL;
               END RECORD;
     END PACK1;

     PACKAGE PACK2 IS
          TYPE UNCONS4 IS LIMITED PRIVATE;
          TYPE UNCONS5 IS LIMITED PRIVATE;
          TYPE UNCONS6 IS LIMITED PRIVATE;
          TYPE UNCONS7 IS LIMITED PRIVATE;
     PRIVATE
          TYPE UNCONS1 (H : INTEGER) IS
               RECORD
                    NULL;
               END RECORD;
          TYPE UNCONS2 (D : INTEGER := 2) IS
               RECORD
                    NULL;
               END RECORD;
          TYPE UNCONS3 IS ARRAY
                    (INTEGER RANGE <>) OF INTEGER;
          TYPE UNCONS4 IS NEW UNCONS1;       -- ERROR: UNCONSTRAINED
          TYPE UNCONS5 IS NEW UNCONS2;       -- OK.
          TYPE UNCONS6 IS NEW UNCONS3;       -- ERROR: UNCONSTRAINED
          TYPE UNCONS7 IS ARRAY              -- ERROR: UNCONSTRAINED
                    (INTEGER RANGE <>) OF INTEGER;
     END PACK2;


BEGIN

     NULL;

END B74105A;
