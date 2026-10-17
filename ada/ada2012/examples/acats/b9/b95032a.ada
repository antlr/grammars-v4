-- B95032A.ADA

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
-- CHECK THAT - OUTSIDE THE BODY OF A TASK - ENTRY NAMES OF THIS TASK
-- HAVE THE FORM OF A SELECTED COMPONENT, WITH THE NAME OF THE TASK
-- OBJECT PREFIXING THE SIMPLE NAME OF ONE OF ITS ENTRIES (OR ENTRY
-- FAMILIES).

-- WEI  3/ 4/82
-- RJK  2/ 1/84     ADDED TO ACVC
-- JWC 6/28/85   RENAMED FROM B950AHA-B.ADA

PROCEDURE B95032A IS

     TYPE I1 IS RANGE 1..1;

     TASK T1 IS
          ENTRY ET1;
          ENTRY ET2 (I1);
     END T1;

     TASK TYPE TT1 IS
          ENTRY ETT1;
          ENTRY ETT2 (I1);
     END TT1;

     OBJ_TT1 : TT1;

     TASK BODY T1 IS
     BEGIN
          ACCEPT ET1;
          ACCEPT ET2 (1);
     END T1;

     TASK BODY TT1 IS
     BEGIN
          ACCEPT ETT1;
          ACCEPT ETT2 (1);
     END TT1;

BEGIN

     T1.ET1; -- OK.
     T1.ET2 (1); -- OK.
     OBJ_TT1.ETT1; -- OK.
     OBJ_TT1.ETT2 (1); -- OK.

     ET1;                -- ERROR: NO PREFIX.
     ET2 (1);            -- ERROR: NO PREFIX.
     ETT1;               -- ERROR: NO PREFIX.
     ETT2 (1);           -- ERROR: NO PREFIX.
     T1'ET1;             -- ERROR: ' INSTEAD OF . .
     T1 (ET1);           -- ERROR: WRONG SYNTAX.
     T1_ET1;             -- ERROR: WRONG SYNTAX.
     ET1 (T1);           -- ERROR: WRONG SYNTAX.
     ET1.T1;             -- ERROR: WRONG SYNTAX.
     ET1'T1;             -- ERROR: WRONG SYNTAX.
     OBJ_TT1'ETT2 (1);   -- ERROR: ' INSTEAD OF . .
     OBJ_TT1 (ETT2 (1)); -- ERROR: WRONG SYNTAX.
     ETT2 (1) (OBJ_TT1); -- ERROR: WRONG SYNTAX.
     ETT2 (1).OBJ_TT1;   -- ERROR: WRONG SYNTAX.
     ETT2 (1)'OBJ_TT1;   -- ERROR: WRONG SYNTAX.

END B95032A;
