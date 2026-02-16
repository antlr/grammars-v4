-- B36102A.ADA

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
-- CHECK THAT UPPER AND LOWER BOUNDS OF DISCRETE RANGES MUST BE THE SAME
-- TYPE, IN ALL CONTEXTS (EXCEPT ENTRIES).

-- CHANGE HISTORY:
--      02 Feb 81   DAT
--      21 Apr 21   RLB     Added error location indicators; split constructs
--                          so only one error per construct.

PROCEDURE B36102A IS

     TYPE STAR IS (SUN, ALGOL, PROCYON, DENEB, ALTAIR, VEGA);
     TYPE DAY IS (MON, TUE, WED, THU, FRI, SAT, SUN);
     TYPE D_BOOL IS NEW BOOLEAN;

     TYPE A1 IS ARRAY (MON .. VEGA) OF DAY;       -- ERROR: VEGA.   {6;1}
     TYPE A2 IS ARRAY (BOOLEAN'(TRUE) ..
                       D_BOOL'(TRUE)) OF DAY;     -- ERROR: D_BOOL. {1:6;1}
     TYPE A3 IS ARRAY (BOOLEAN RANGE TRUE ..
                       D_BOOL'(FALSE)) OF DAY;    -- ERROR: D_BOOL. {1:6;1}

     DBF : CONSTANT D_BOOL := FALSE;
     DBT : CONSTANT D_BOOL := TRUE;
     BF : CONSTANT BOOLEAN := FALSE;
     BT : CONSTANT BOOLEAN := TRUE;

     TYPE REC1 (DISC : BOOLEAN) IS RECORD
          CASE DISC IS
               WHEN BF .. DBF => NULL;            -- ERROR: DBF.    {1:11;1}
               WHEN OTHERS => NULL;
          END CASE;
     END RECORD;
     TYPE REC2 (DISC : BOOLEAN) IS RECORD
          CASE DISC IS
               WHEN BOOLEAN RANGE TRUE .. DBT =>  -- ERROR: DBT.    {1:11;1}
                    NULL;
               WHEN OTHERS => NULL;
          END CASE;
     END RECORD;

     TYPE BA IS ARRAY (BOOLEAN RANGE <>) OF D_BOOL;
     TYPE BB IS ARRAY (D_BOOL RANGE <>) OF BOOLEAN;

     B : BB (D_BOOL RANGE FALSE .. TRUE);         -- OK.            {6;1}
     X1 : BB (D_BOOL RANGE DBF .. BT);            -- ERROR: BT.     {6;1}
     X2 : BA (BF .. DBT);                         -- ERROR: DBT.    {6;1}
     TYPE T1 IS NEW BB (BF .. DBT);               -- ERROR: BF.     {6;1}
     TYPE T2 IS NEW BA (BOOLEAN RANGE BT .. DBT); -- ERROR: DBT.    {6;1}

BEGIN

     FOR I IN BT .. DBF LOOP                      -- ERROR: BT .. DBF. {6;1}
          NULL;
     END LOOP;

     FOR I IN BOOLEAN RANGE DBT .. BT LOOP        -- ERROR: DBT.    {6;1}
          NULL;
     END LOOP;

     CASE DBT IS
          WHEN BT .. DBT => NULL ;                -- ERROR: BT.     {1:6;1}
          WHEN OTHERS => NULL; 
     END CASE;
     CASE DBT IS
          WHEN D_BOOL RANGE BF .. DBF => NULL;    -- ERROR: BF.     {1:6;1}
          WHEN OTHERS => NULL; 
     END CASE;

     B := (DBF .. BT => FALSE);                   -- ERROR: BT.     {6;1}
     B := (D_BOOL RANGE BF .. DBT => TRUE);       -- ERROR: BF.     {6;1}
     B := B (D_BOOL RANGE DBF .. BT);             -- ERROR: BT.     {6;1}
     B := B (BF .. DBT);                          -- ERROR: BF.     {6;1}

END B36102A;
