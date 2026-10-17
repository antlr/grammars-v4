-- B32103A.ADA

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
-- CHECK THAT CONSTANT OBJECT DECLARATIONS MUST HAVE AN EXPLICIT
-- INITIALIZATION, EVEN WHEN DEFAULT VALUES EXIST FOR THE TYPE
-- OR FOR EACH COMPONENT.

-- DAT 3/17/81
-- SPS 2/4/83

PROCEDURE B32103A IS

     TYPE TA IS ACCESS INTEGER;
     TYPE AA IS ARRAY (BOOLEAN) OF TA;
     TYPE REC IS
          RECORD
               C : INTEGER := 1;
          END RECORD;

     CX : CONSTANT INTEGER;                  -- ERROR: UNINITIALIZED.
     CY : CONSTANT BOOLEAN;                  -- ERROR: UNINITIALIZED.
     CZ : CONSTANT CHARACTER;                -- ERROR: UNINITIALIZED.
     C1 : CONSTANT TA;                       -- ERROR: UNINITIALIZED.
     C2 : CONSTANT AA;                       -- ERROR: UNINITIALIZED.
     C3 : CONSTANT REC;                      -- ERROR: UNINITIALIZED.
     C4 : CONSTANT ARRAY (1..1) OF TA;       -- ERROR: UNINITIALIZED.

     PACKAGE P IS

          TYPE P IS PRIVATE;
          TYPE ENUM IS (A);
          TYPE TA IS ACCESS P;
          TYPE AA IS ARRAY (BOOLEAN) OF TA;
          TYPE REC IS
               RECORD
                    C : INTEGER := 1;
               END RECORD;

          C1 : CONSTANT TA;                  -- ERROR: UNINITIALIZED.
          C2 : CONSTANT AA;                  -- ERROR: UNINITIALIZED.
          C3 : CONSTANT REC;                 -- ERROR: UNINITIALIZED.
          C4 : CONSTANT ARRAY (1..1) OF TA;  -- ERROR: UNINITIALIZED.
          C5 : CONSTANT ENUM;                -- ERROR: UNINITIALIZED.

     PRIVATE
          TYPE P IS ARRAY (1..1) OF BOOLEAN;
     END P;

BEGIN
     NULL;
END B32103A;
