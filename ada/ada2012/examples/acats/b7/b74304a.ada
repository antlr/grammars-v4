-- B74304A.ADA

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
--    CHECK THAT A DEFERRED CONSTANT CANNOT BE USED AS AN
--    INITIAL VALUE FOR AN OBJECT OR CONSTANT DECLARATION UNTIL AFTER
--    THE FULL CONSTANT DECLARATION.

-- DAT 4/6/81
-- RM 5/21/81
-- SPS 8/23/82
-- SPS 2/10/83
-- SPS 10/20/83

PROCEDURE B74304A IS
 
     PACKAGE PK IS
          TYPE T1 IS PRIVATE;
          C1 : CONSTANT T1;                   -- OK.

          PACKAGE P2 IS
               TYPE T2 IS PRIVATE;
               C22 : CONSTANT T2;             -- OK.
               C23 : CONSTANT T2;             -- OK.
               C24 : CONSTANT T2;             -- OK.
               C25 : CONSTANT T2;             -- OK.
               C26 : CONSTANT T2;             -- OK.
               C27 : CONSTANT T2;             -- OK.
          PRIVATE
               TYPE T2 IS ACCESS INTEGER;
               C22 : CONSTANT T2 := NULL;
               C23 : CONSTANT T2 := C22;      -- OK.
               C24 : CONSTANT T2 := C26;      -- ERROR: C26 UNDEF.
               C26 : CONSTANT T2 := NULL;
               C27 : CONSTANT T2 := C25;      -- ERROR: C25 UNDEF. 
               C25 : CONSTANT T2 := C22;      -- OK.
          END P2;                            

          USE P2;
     PRIVATE

          TYPE T1 IS ACCESS INTEGER;

          PACKAGE P5 IS
               TYPE T1 IS PRIVATE;
               C4B, C4C : CONSTANT T1;        -- OK.
          PRIVATE
               TYPE T1 IS NEW PK.T1;          -- OK.
               C4C : CONSTANT T1 := NULL;     -- OK.
               C4B : CONSTANT T1 := T1(C1);   -- ERROR: C1 UNDEF.   
               C4D : CONSTANT PK.T1 := C1;    -- ERROR: C1 UNDEF.   
          END P5;

          V7 : T1 := C1;                      -- ERROR: C1 UNDEF.   
          V8 : CONSTANT T1 := C1;             -- ERROR: C1 UNDEF.   
          C1 : CONSTANT T1 := NULL;           -- OK.
     END PK;
BEGIN
     NULL;
END B74304A;
