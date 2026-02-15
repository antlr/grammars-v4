-- B36103A.ADA

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
-- CHECK THAT UPPER AND LOWER BOUNDS OF A DISCRETE RANGE MUST HAVE THE
-- SAME TYPE AS THE TYPE MARK, WHERE BOTH BOUNDS ARE OF THE SAME TYPE.

-- DAT 2/3/81

PROCEDURE B36103A IS

     SUBTYPE B1 IS BOOLEAN RANGE TRUE .. TRUE;
     BT : B1 := TRUE;

     TYPE D_BOOL IS NEW BOOLEAN;
     SUBTYPE DB1 IS D_BOOL RANGE TRUE .. TRUE;
     DBT : DB1 := TRUE;

     TYPE AA IS ARRAY (B1 RANGE
          D_BOOL'(TRUE) .. D_BOOL'(TRUE))  -- ERROR: D_BOOL NOT B1.
          OF BOOLEAN;

     TYPE BA IS ARRAY (BOOLEAN RANGE
          D_BOOL'(FALSE) .. D_BOOL'(TRUE)) -- ERROR: D_BOOL NOT BOOLEAN.
          OF BOOLEAN;

     TYPE AB IS ARRAY (D_BOOL RANGE
          BOOLEAN'(FALSE)..BOOLEAN'(TRUE)) -- ERROR: BOOLEAN NOT D_BOOL.
          OF BOOLEAN;

BEGIN

     FOR I IN BOOLEAN RANGE DBT .. DBT LOOP -- ERROR: DBT NOT BOOLEAN.
          NULL;
     END LOOP;

     FOR I IN B1 RANGE DBT .. DBT LOOP     -- ERROR: DBT NOT TYPE B1.
          NULL;
     END LOOP;

     FOR I IN D_BOOL RANGE BT .. BT LOOP   -- ERROR: BT NOT D_BOOL.
          NULL;
     END LOOP;

     FOR I IN DB1 RANGE BT .. BT LOOP      -- ERROR: BT NOT DB1.
          NULL;
     END LOOP;

END B36103A;
