-- B83E11A.ADA

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
--     CHECK THAT AN ENTRY DECLARATION CANNOT DECLARE AN ENTRY OR AN
--     ENTRY FAMILY WITH DUPLICATE FORMAL PARAMETER NAMES.

-- HISTORY:
--     DHH 09/14/88  CREATED ORIGINAL TEST.

PROCEDURE B83E11A IS

     TASK T1 IS
          ENTRY DUP(PARAM1 : BOOLEAN;
                    PARAM1 : INTEGER;                         -- ERROR:
                    PARAM1 : CHARACTER;                       -- ERROR:
                    PARAM3 : STRING;
                    PARAM1 : STRING;                          -- ERROR:
                    PARAM2 : INTEGER;
                    PARAM1 : INTEGER);                        -- ERROR:
     END T1;

     TASK TYPE T2 IS
          ENTRY DUP(PARAM1 : BOOLEAN;
                    PARAM3 : STRING;
                    PARAM1 : STRING;                          -- ERROR:
                    PARAM2 : INTEGER;
                    PARAM1 : INTEGER);                        -- ERROR:
     END T2;

     TYPE COLOR IS (RED, YELLOW, BLUE);

     TASK T3 IS
          ENTRY DUP(COLOR)
                   (PARAM1 : INTEGER;
                    PARAM2 : INTEGER;
                    PARAM1 : INTEGER);                        -- ERROR:
     END T3;

     TASK BODY T1 IS
     BEGIN
          ACCEPT DUP(PARAM1 : BOOLEAN;
                     PARAM1 : INTEGER;          -- OPTIONAL ERR MESSAGE.
                     PARAM1 : CHARACTER;        -- OPTIONAL ERR MESSAGE.
                     PARAM3 : STRING;
                     PARAM1 : STRING;           -- OPTIONAL ERR MESSAGE.
                     PARAM2 : INTEGER;
                     PARAM1 : INTEGER);         -- OPTIONAL ERR MESSAGE.
     END T1;

     TASK BODY T2 IS
     BEGIN
          ACCEPT DUP(PARAM1 : BOOLEAN;
                     PARAM3 : STRING;
                     PARAM1 : STRING;           -- OPTIONAL ERR MESSAGE.
                     PARAM2 : INTEGER;
                     PARAM1 : INTEGER);         -- OPTIONAL ERR MESSAGE.
     END T2;

     TASK BODY T3 IS
     BEGIN
          ACCEPT DUP(RED)
                    (PARAM1 : INTEGER;
                     PARAM2 : INTEGER;
                     PARAM1 : INTEGER);         -- OPTIONAL ERR MESSAGE.
          ACCEPT DUP(YELLOW)
                    (PARAM1 : INTEGER;
                     PARAM2 : INTEGER;
                     PARAM1 : INTEGER);         -- OPTIONAL ERR MESSAGE.
          ACCEPT DUP(BLUE)
                    (PARAM1 : INTEGER;
                     PARAM2 : INTEGER;
                     PARAM1 : INTEGER);         -- OPTIONAL ERR MESSAGE.
     END T3;

BEGIN
     NULL;
END B83E11A;
